const std = @import("std");

pub fn main() !void {
    //
    // Set up allocator
    //

    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();

    //
    // Read UnicodeData.txt and normalize newlines
    //

    const contents = try std.fs.cwd().readFileAlloc(allocator, "UnicodeData.txt", 3 * 1024 * 1024);
    const normalized = try std.mem.replaceOwned(u8, allocator, contents, "\r\n", "\n");

    allocator.free(contents);
    defer allocator.free(normalized);

    //
    // Load decomposition map
    //

    var decomp_bin = try std.fs.cwd().openFile("decomp.bin", .{});
    defer decomp_bin.close();

    var decomp_br = std.io.bufferedReader(decomp_bin.reader());
    var decomp_map = try loadDecompMap(allocator, decomp_br.reader());
    defer {
        var it = decomp_map.iterator();
        while (it.next()) |entry| allocator.free(entry.value_ptr.*);
        decomp_map.deinit();
    }

    //
    // Load CCC map
    //

    // var ccc_map: std.AutoHashMap(u32, u8) = try loadCCC(allocator);
    // defer ccc_map.deinit();

    var ccc_in = try std.fs.cwd().openFile("ccc.bin", .{});
    defer ccc_in.close();

    var ccc_br = std.io.bufferedReader(ccc_in.reader());
    var ccc_map = try loadCccMap(allocator, ccc_br.reader());
    defer ccc_map.deinit();

    //
    // Set up FCD map
    //

    var fcd_map = std.AutoHashMap(u32, u16).init(allocator);
    defer fcd_map.deinit();

    //
    // Iterate over lines and find combining classes
    //

    var line_iter = std.mem.splitScalar(u8, normalized, '\n');

    while (line_iter.next()) |line| {
        if (line.len == 0) continue;

        var fields = std.ArrayList([]const u8).init(allocator);
        defer fields.deinit();

        var field_iter = std.mem.splitScalar(u8, line, ';');
        while (field_iter.next()) |field| {
            try fields.append(field);
        }

        const code_point = try std.fmt.parseInt(u32, fields.items[0], 16);

        if ((0x3400 <= code_point and code_point <= 0x4DBF) // CJK ext A
        or (0x4E00 <= code_point and code_point <= 0x9FFF) // CJK
        or (0xAC00 <= code_point and code_point <= 0xD7A3) // Hangul
        or (0xD800 <= code_point and code_point <= 0xDFFF) // Surrogates
        or (0xE000 <= code_point and code_point <= 0xF8FF) // Private use
        or (0x17000 <= code_point and code_point <= 0x187F7) // Tangut
        or (0x18D00 <= code_point and code_point <= 0x18D08) // Tangut suppl
        or (0x20000 <= code_point and code_point <= 0x2A6DF) // CJK ext B
        or (0x2A700 <= code_point and code_point <= 0x2B738) // CJK ext C
        or (0x2B740 <= code_point and code_point <= 0x2B81D) // CJK ext D
        or (0x2B820 <= code_point and code_point <= 0x2CEA1) // CJK ext E
        or (0x2CEB0 <= code_point and code_point <= 0x2EBE0) // CJK ext F
        or (0x30000 <= code_point and code_point <= 0x3134A) // CJK ext G
        or (0xF0000 <= code_point and code_point <= 0xFFFFD) // Plane 15 private use
        or (0x10_0000 <= code_point and code_point <= 0x10_FFFD) // Plane 16 private use
        ) {
            continue;
        }

        const decomp: []const u32 = decomp_map.get(code_point) orelse continue;

        const first_ccc: u8 = ccc_map.get(decomp[0]) orelse 0;
        const last_ccc: u8 = ccc_map.get(decomp[decomp.len - 1]) orelse 0;

        const fcd: u16 = @as(u16, first_ccc) << 8 | @as(u16, last_ccc);
        if (fcd == 0) continue;

        try fcd_map.put(code_point, fcd);
    }

    //
    // Write FCD map to binary file
    //

    var fcd_file = try std.fs.cwd().createFile("fcd.bin", .{ .truncate = true });
    defer fcd_file.close();

    var fcd_bw = std.io.bufferedWriter(fcd_file.writer());
    try saveFcdMap(&fcd_map, fcd_bw.writer());
    try fcd_bw.flush();

    //
    // Write FCD map to JSON file
    //

    const output_file = try std.fs.cwd().createFile("fcd.json", .{ .truncate = true });
    defer output_file.close();

    var ws = std.json.writeStream(output_file.writer(), .{ .whitespace = .indent_2 });
    try ws.beginObject();

    var map_iter = fcd_map.iterator();
    while (map_iter.next()) |entry| {
        const key_str = try std.fmt.allocPrint(allocator, "{}", .{entry.key_ptr.*});
        try ws.objectField(key_str);
        try ws.write(entry.value_ptr.*);

        allocator.free(key_str);
    }

    try ws.endObject();
}

//
// Types
//

const CccEntry = packed struct {
    key: u32,
    value: u8,
};

const DecompEntryHeader = packed struct {
    key: u32,
    len: u8,
};

const DecompMap = struct {
    map: std.AutoHashMap(u32, []const u32),
    allocator: std.mem.Allocator,

    pub fn deinit(self: *DecompMap) void {
        var it = self.map.iterator();
        while (it.next()) |e| self.allocator.free(e.value_ptr.*);
        self.map.deinit();
    }
};

const DecompMapHeader = packed struct {
    count: u32,
    total_bytes: u32,
};

const FcdEntry = packed struct {
    key: u32,
    value: u16,
};

//
// Constants
//

const MAX_BYTES: u32 = 1024 * 1024; // 1 MiB

//
// Load functions
//

fn loadCCC(allocator: std.mem.Allocator) !std.AutoHashMap(u32, u8) {
    const path = "ccc.json";
    const json_bytes = try std.fs.cwd().readFileAlloc(allocator, path, 20 * 1024);
    defer allocator.free(json_bytes);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_bytes, .{});
    errdefer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.ExpectedTopLevelObject;

    var map = std.AutoHashMap(u32, u8).init(allocator);
    errdefer map.deinit();

    var it = root.object.iterator();
    while (it.next()) |entry| {
        const cp = try std.fmt.parseInt(u32, entry.key_ptr.*, 10);

        const val = entry.value_ptr.*;
        if (val != .integer) return error.ExpectedNumber;

        const ccc: u8 = @intCast(val.integer); // From i64
        try map.put(cp, ccc);
    }

    parsed.deinit();
    return map;
}

fn loadCccMap(allocator: std.mem.Allocator, reader: anytype) !std.AutoHashMap(u32, u8) {
    const count = try reader.readInt(u32, .little);
    const bytes_needed: usize = @as(usize, count) * @sizeOf(CccEntry);
    if (bytes_needed > MAX_BYTES) return error.FileTooLarge;

    const payload = try allocator.alloc(u8, bytes_needed);
    defer allocator.free(payload);

    try reader.readNoEof(payload);
    const entries = std.mem.bytesAsSlice(CccEntry, payload);
    std.debug.assert(entries.len == count);

    var map = std.AutoHashMap(u32, u8).init(allocator);
    try map.ensureTotalCapacity(count);

    for (entries) |e| {
        const key = std.mem.littleToNative(u32, e.key);
        const value = e.value; // u8 has no endianness
        try map.put(key, value);
    }

    return map;
}

fn loadDecompJson(allocator: std.mem.Allocator) !DecompMap {
    const path = "decomp.json";
    const json_bytes = try std.fs.cwd().readFileAlloc(allocator, path, 66 * 1024);
    defer allocator.free(json_bytes);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_bytes, .{});
    errdefer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.ExpectedTopLevelObject;

    var map = std.AutoHashMap(u32, []const u32).init(allocator);
    errdefer {
        var it = map.iterator();
        while (it.next()) |entry| allocator.free(entry.value_ptr.*);
        map.deinit();
    }

    var it = root.object.iterator();
    while (it.next()) |entry| {
        const cp = try std.fmt.parseInt(u32, entry.key_ptr.*, 10);

        const val = entry.value_ptr.*;
        if (val != .array) return error.ExpectedArray;

        const arr = val.array.items;
        const slice = try allocator.alloc(u32, arr.len);

        for (arr, 0..) |elem, i| {
            if (elem != .integer) return error.ExpectedNumber;
            const as_u32: u32 = @intCast(elem.integer); // From i64
            slice[i] = as_u32;
        }

        try map.put(cp, slice);
    }

    parsed.deinit();
    return DecompMap{ .map = map, .allocator = allocator };
}

fn loadDecompMap(allocator: std.mem.Allocator, reader: anytype) !std.AutoHashMap(u32, []u32) {
    const main_header = try reader.readStruct(DecompMapHeader);
    const count = std.mem.littleToNative(u32, main_header.count);

    const total_bytes = std.mem.littleToNative(u32, main_header.total_bytes);
    if (total_bytes > MAX_BYTES) return error.FileTooLarge;

    const payload = try allocator.alloc(u8, total_bytes);
    defer allocator.free(payload);

    try reader.readNoEof(payload);

    var map = std.AutoHashMap(u32, []u32).init(allocator);
    try map.ensureTotalCapacity(count);

    var offset: usize = 0;
    var n: u32 = 0;

    while (n < count) : (n += 1) {
        const header = std.mem.bytesToValue(
            DecompEntryHeader,
            payload[offset..][0..@sizeOf(DecompEntryHeader)],
        );
        offset += @sizeOf(DecompEntryHeader);

        const key = std.mem.littleToNative(u32, header.key);

        const values_len = header.len; // u8 has no endianness
        const value_bytes = values_len * @sizeOf(u32);

        const vals = try allocator.alloc(u32, values_len);
        errdefer allocator.free(vals);

        const payload_vals = std.mem.bytesAsSlice(u32, payload[offset..][0..value_bytes]);
        for (payload_vals, vals) |src, *dst| {
            dst.* = std.mem.littleToNative(u32, src);
        }

        try map.put(key, vals);
        offset += value_bytes;
    }

    return map;
}

//
// Save functions
//

fn saveFcdMap(map: *const std.AutoHashMap(u32, u16), writer: anytype) !void {
    try writer.writeInt(u32, @intCast(map.count()), .little);

    var it = map.iterator();
    while (it.next()) |kv| {
        const e = FcdEntry{
            .key = std.mem.nativeToLittle(u32, kv.key_ptr.*),
            .value = std.mem.nativeToLittle(u16, kv.value_ptr.*),
        };
        try writer.writeStruct(e);
    }
}
