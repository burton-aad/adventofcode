const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

// Button A: X+a, Y+c
// Button B: X+b, Y+d
// Prize: P=..., Q=...
// -> ax + by = p
//    cx + dy = q
//
//        p - ax   q - cx
// -> y = ------ = ------
//           b        d
//
// -> (dp - bq) + (bc - ad)x = 0
//
//        bq - dp      p - ax
// -> x = -------, y = ------
//        bc - ad         b
fn calc_tokens(a: usize, b: usize, c: usize, d: usize, p: usize, q: usize) usize {
    const xnum = utils.diff(usize, b*q, d*p);
    const xden = utils.diff(usize, b*c, a*d);
    if (xnum % xden == 0) {
        const x = xnum / xden;
        const ynum = p - a*x;
        if (ynum % b == 0) {
            const y = ynum / b;
            return 3*x + y;
        }
    }
    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(argv[1], allocator);
    defer utils.free_lines(lines);
    // for (lines.items) |l| print("lines {s}\n", .{l});

    var i: usize = 0;
    var sum: usize = 0;
    var sum2: usize = 0;
    while (i < lines.items.len) {
        var it = std.mem.splitAny(u8, lines.items[i], ",+");
        _ = it.next();
        const a = try utils.int(usize, it.next().?);
        _ = it.next();
        const c = try utils.int(usize, it.next().?);
        i += 1;

        it = std.mem.splitAny(u8, lines.items[i], ",+");
        _ = it.next();
        const b = try utils.int(usize, it.next().?);
        _ = it.next();
        const d = try utils.int(usize, it.next().?);
        i += 1;

        it = std.mem.splitAny(u8, lines.items[i], ",=");
        _ = it.next();
        const p = try utils.int(usize, it.next().?);
        _ = it.next();
        const q = try utils.int(usize, it.next().?);
        i += 2;

        sum += calc_tokens(a, b, c, d, p, q);
        const extra_2: usize = 10_000_000_000_000;
        sum2 += calc_tokens(a, b, c, d, p + extra_2, q + extra_2);
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}

