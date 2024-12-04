const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var list1 = std.ArrayList(u64).init(allocator);
    defer list1.deinit();
    var list2 = std.ArrayList(u64).init(allocator);
    defer list2.deinit();
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        var it = std.mem.splitSequence(u8, line, "   ");
        try list1.append(try std.fmt.parseInt(u64, it.first(), 10));
        try list2.append(try std.fmt.parseInt(u64, it.next().?, 10));
    }

    std.sort.block(u64, list1.items, {}, std.sort.asc(u64));
    std.sort.block(u64, list2.items, {}, std.sort.asc(u64));

    var m = try utils.Counter(u64, list2.items, allocator);
    defer m.deinit();

    var sum: u64 = 0;
    var sum2: u64 = 0;
    for (list1.items, list2.items) |e1, e2| {
        sum += if (e1 > e2) e1 - e2 else e2 - e1;
        sum2 += e1 * (m.get(e1) orelse 0);
    }
    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}

