const std = @import("std");

const DecompMap = struct {
    map: std.AutoHashMap(u32, []const u32),
    allocator: std.mem.Allocator,

    pub fn deinit(self: *DecompMap) void {
        var it = self.map.iterator();
        while (it.next()) |e| self.allocator.free(e.value_ptr.*);
        self.map.deinit();
    }
};

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

    var decomps: DecompMap = try loadDecomp(allocator);
    defer decomps.deinit();

    //
    // Load CCC map
    //

    var ccc_map: std.AutoHashMap(u32, u8) = try loadCCC(allocator);
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

        const decomp: []const u32 = decomps.map.get(code_point) orelse continue;

        const first_ccc: u8 = ccc_map.get(decomp[0]) orelse 0;
        const last_ccc: u8 = ccc_map.get(decomp[decomp.len - 1]) orelse 0;

        const fcd: u16 = @as(u16, first_ccc) << 8 | @as(u16, last_ccc);
        if (fcd == 0) continue;

        try fcd_map.put(code_point, fcd);
    }

    //
    // Write FCD map to a JSON file
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

fn loadDecomp(allocator: std.mem.Allocator) !DecompMap {
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
