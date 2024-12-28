const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Lock = [5]usize;
const Key = [5]usize;

fn compatible(l: Lock, k: Key) bool {
    for (l, k) |lc, kc| {
        if (lc + kc > 7)
            return false;
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
    var locks = std.ArrayList(Lock).init(allocator);
    var keys = std.ArrayList(Key).init(allocator);
    defer{
        locks.deinit();
        keys.deinit();
    }
    while (try utils.readline(input.reader(), &buf)) |first_line| {
        // print("line : '{s}'\n", .{first_line});
        const l = first_line[0] == '#';
        var lk = [_]usize{ if (l) 1 else 0 } ** 5;
        while (try utils.readline(input.reader(), &buf)) |line| {
            // print("line : '{s}'\n", .{line});
            if (line.len == 0) break;
            for (line, 0..) |c, i| {
                if (c == '#')
                    lk[i] += 1;
            }
        }
        if (l) {
            try locks.append(lk);
        }
        else
            try keys.append(lk);
    }

    var sum: usize = 0;
    for (locks.items) |l| {
        for (keys.items) |k| {
            if (compatible(l, k))
                sum += 1;
        }
    }
    print("Part 1: {}\n", .{ sum });
}

