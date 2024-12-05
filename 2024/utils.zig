const std = @import("std");

pub fn print(comptime format: []const u8, args: anytype) void
{
    std.io.getStdOut().writer().print(format, args) catch {};
}

pub fn readline(reader: anytype, buffer: []u8) !?[]u8 {
    return try reader.readUntilDelimiterOrEof(buffer, '\n');
}

pub fn Counter(comptime T: type, array: []T, alloc: std.mem.Allocator) !std.AutoHashMap(T, usize) {
    var map = std.AutoHashMap(T, usize).init(alloc);
    for (array) |e| {
        const r = try map.getOrPut(e);
        if (r.found_existing) r.value_ptr.* += 1 else r.value_ptr.* = 1;
    }
    return map;
}

pub fn int(comptime T: type, str: []const u8) !T
{
    if (std.mem.startsWith(u8, str, "0x"))
        return try std.fmt.parseInt(T, str, 16);
    return try std.fmt.parseInt(T, str, 10);
}

pub fn read_lines(path: [*:0]const u8, allocator: std.mem.Allocator) !std.ArrayList([]u8) {
    var lines = std.ArrayList([]u8).init(allocator);

    const input = try std.fs.cwd().openFileZ(path, .{});
    defer input.close();

    while (try input.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.pow(usize, 2, 24))) |line| {
        try lines.append(line);
    }

    return lines;
}

pub fn free_lines(lines: std.ArrayList([]u8)) void {
    for (lines.items) |l| {
        lines.allocator.free(l);
    }
    lines.deinit();
}
