const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const sdir = enum { none, inc, dec };

fn check_report(line: []const u64, ignore: ?usize) bool
{
    var inc = sdir.none;
    var cur: ?u64 = null;
    for (line, 0..) |e, i| {
        if (ignore != null and i == ignore.?)
            continue;
        if (cur == null) {
            cur = e;
            continue;
        }
        switch (inc) {
            sdir.inc => { if (e <= cur.? or e - cur.? > 3 ) return false; },
            sdir.dec => { if (cur.? <= e or cur.? - e > 3 ) return false; },
            sdir.none => {
                if (e < cur.?) {
                    inc = sdir.dec;
                    if (cur.? - e > 3)
                        return false;
                }
                else if (e > cur.?) {
                    inc = sdir.inc;
                    if (e - cur.? > 3)
                        return false;
                }
                else
                    return false;
            },
        }
        cur = e;
    }

    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var safe: u64 = 0;
    var safe2: u64 = 0;
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        var arr = std.ArrayList(u64).init(allocator);
        defer arr.deinit();
        var it = std.mem.splitSequence(u8, line, " ");
        while (it.next()) |e| {
            try arr.append(try utils.int(u64, e));
        }

        // O(n^2). Could be better with error info but ok enough.
        if (check_report(arr.items, null)) {
            safe += 1;
            safe2 += 1;
        }
        else {
            for (0..arr.items.len) |n| {
                if (check_report(arr.items, n)) {
                    safe2 += 1;
                    break;
                }
            }
        }
    }

    print("Part 1: {}\n", .{ safe });
    print("Part 2: {}\n", .{ safe2 });
}

