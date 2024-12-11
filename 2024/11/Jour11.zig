const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

fn rule(stone: usize) [2]?usize {
    if (stone == 0) {
        return .{ 1, null};
    }

    const l = std.math.log10(stone);
    if (l % 2 != 0) {
        const p = std.math.pow(usize, 10, (l+1) / 2);
        return .{ stone / p, stone % p };
    }
    else
        return .{ stone * 2024, null };
}

fn blink(stones: *std.ArrayList(usize)) !void {
    var i: usize = 0;
    while (i < stones.items.len) : (i += 1) {
        const s = rule(stones.items[i]);
        stones.items[i] = s[0].?;
        if (s[1]) |e| {
            i += 1;
            try stones.insert(i, e);
        }
    }
}

fn memo_blink(stone: usize, times: usize, memo: *std.AutoHashMap([2]usize, usize)) !usize {
    if (times == 0)
        return 1;

    const k = .{stone, times};
    if (!memo.contains(k)) {
        var v : usize = 0;
        const s = rule(stone);
        v += try memo_blink(s[0].?, times-1, memo);
        if (s[1]) |e| {
            v += try memo_blink(e, times-1, memo);
        }
        try memo.put(k, v);
    }

    return memo.get(k).?;
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) { return error.InvalidParam; }
    const input = try std.fs.cwd().openFileZ(argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var stones = std.ArrayList(usize).init(allocator);
    defer stones.deinit();
    while (try utils.readline(input.reader(), &buf)) |line| {
        var it = std.mem.splitSequence(u8, std.mem.trim(u8, line, "\r"), " ");
        while (it.next()) |e| try stones.append(try utils.int(usize, e));
    }

    // print("test {any}\n", .{ stones.items });
    var memo = std.AutoHashMap([2]usize, usize).init(allocator);
    defer memo.deinit();
    var sum: usize = 0;
    var sum2: usize = 0;
    for (stones.items) |s| {
        sum += try memo_blink(s, 25, &memo);
        sum2 += try memo_blink(s, 75, &memo);
    }
    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });

    // print("memo size {}\n", .{ memo.count() });
    // print("Time : {} ms\n", .{ timer.read() / 1000000 });
}
