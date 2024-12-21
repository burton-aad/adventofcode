const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const limit1 = 1024;
const size = 71;
var mem = [_][size]u8{ [_]u8{'.'} ** size } ** size;

const Pos = struct { x: usize = 0, y: usize = 0 };
const Path = std.ArrayList(Pos);

fn neighbor4(p: Pos) [4]?Pos {
    return .{
        if (p.x > 0) .{ .x = p.x-1, .y = p.y } else null,
        if (p.y > 0) .{ .x = p.x, .y = p.y-1 } else null,
        if (p.y < size-1) .{ .x = p.x, .y = p.y+1 } else null,
        if (p.x < size-1) .{ .x = p.x+1, .y = p.y } else null,
    };
}

fn compare_path(context: void, a: Path, b: Path) std.math.Order {
    _ = context;
    return std.math.order(a.items.len, b.items.len);
}

fn dijk(allocator: std.mem.Allocator) !Path {
    const start = Pos{.x = 0, .y = 0};
    const end = Pos{.x = size-1, .y = size-1};
    var q = std.PriorityQueue(Path, void, compare_path).init(allocator, {});
    var view = std.AutoHashMap(Pos, usize).init(allocator);
    defer {
        for (q.items) |*p| p.deinit();
        q.deinit();
        view.deinit();
    }

    var pstart = Path.init(allocator);
    try pstart.append(start);
    try q.add(pstart);
    try view.put(start, 1);

    while (q.removeOrNull()) |*p| {
        const cur = p.items[p.items.len-1];
        if (cur.x == end.x and cur.y == end.y) {
            return p.*;
        }
        defer @constCast(p).deinit();
        if (view.contains(cur) and view.get(cur).? < p.items.len) continue;

        for (neighbor4(cur)) |np_| {
            if (np_ == null) continue;
            const np = np_.?;
            if (mem[np.y][np.x] == '#') continue;
            const r = try view.getOrPut(np);
            if (!r.found_existing) {
                var c = try p.clone();
                try c.append(np);
                try q.add(c);
            }
        }
    }

    return error.NoPath;
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var coord = std.ArrayList(Pos).init(allocator);
    defer coord.deinit();
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        var it = std.mem.splitSequence(u8, line, ",");
        try coord.append(.{.x = try utils.int(u8, it.next().?), .y = try utils.int(u8, it.next().?)});
    }

    for (0..limit1) |i| {
        const c = coord.items[i];
        mem[c.y][c.x] = '#';
    }

    // print("mem :\n", .{});
    // for (mem) |l| print("{s}\n", .{l});

    var path = try dijk(allocator);
    defer path.deinit();
    print("Part 1: {}\n", .{ path.items.len-1 });

    var start: usize = limit1;
    var end: usize = coord.items.len;
    while (start < end) {
        const mid = (start + end) / 2;
        for (start..mid) |i| mem[coord.items[i].y][coord.items[i].x] = '#';
        if (dijk(allocator)) |p| {
            p.deinit();
            start = mid;
        }
        else |_| {
            for (start..mid) |i| mem[coord.items[i].y][coord.items[i].x] = '.';
            end = mid - 1;
        }
    }
    print("Part 2: {},{}\n", .{ coord.items[start].x, coord.items[start].y });
}

