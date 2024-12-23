const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;


fn next_secret(secret: usize) usize {
    const prune = 16777216;
    var s = secret;
    s = ((s * 64) ^ s) % prune;
    s = ((s / 32) ^ s) % prune;
    s = ((s * 2048) ^ s) % prune;
    return s;
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    const STEP = 19;
    const SHIFT = 9;
    const DATA_SIZE = comptime std.math.pow(usize, STEP, 4);

    var buf: [1000]u8 = undefined;
    var sum: usize = 0;
    var buy_bananas = [_]usize{ 0 } ** DATA_SIZE;
    var cur_prices = [_]bool{ false } ** DATA_SIZE;
    while (try utils.readline(input.reader(), &buf)) |line| {
        @memset(&cur_prices, false);
        var s = try utils.int(usize, line);
        var p = s % 10;
        var idx = [4]usize{0, 0, 0, 0};
        for (0..2000) |i| {
            const ns = next_secret(s);
            const np = ns % 10;
            const v = np + SHIFT - p;
            idx[0] = idx[1] * STEP + v;
            idx[1] = idx[2] * STEP + v;
            idx[2] = idx[3] * STEP + v;
            idx[3] = v;
            if (i > 2 and !cur_prices[idx[0]]) {
                cur_prices[idx[0]] = true;
                buy_bananas[idx[0]] += np;
            }
            s = ns;
            p = np;
        }
        // print("{s} : {}\n", .{line, s});
        sum += s;
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ std.mem.max(usize, &buy_bananas) });
}

