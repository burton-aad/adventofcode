const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Pos = struct { x: usize = 0, y: usize = 0 };

const Path = struct {
    path: std.ArrayList(u8),
    pos: Pos,
    dir: u8,
    score: usize,

    const Self = @This();

    pub fn init(pos: Pos, allocator: std.mem.Allocator) Path {
        return .{
            .path = std.ArrayList(u8).init(allocator),
            .pos = pos,
            .dir = '>',
            .score = 0,
        };
    }

    pub fn clone(self: Self) !Path {
        return .{
            .path = try self.path.clone(),
            .pos = self.pos,
            .dir = self.dir,
            .score = self.score,
        };
    }

    pub fn deinit(self: Self) void {
        self.path.deinit();
    }
};

fn next_pos(p: Pos, d: u8) Pos {
    return switch (d) {
        '^' => .{.x = p.x, .y = p.y-1},
        'v' => .{.x = p.x, .y = p.y+1},
        '>' => .{.x = p.x+1, .y = p.y},
        '<' => .{.x = p.x-1, .y = p.y},
        else => unreachable,
    };
}

fn compare_path(context: void, a: Path, b: Path) std.math.Order {
    _ = context;
    return std.math.order(a.score, b.score);
}

fn dijk(lines: [][]u8, start: Pos, end: Pos, allocator: std.mem.Allocator) !std.ArrayList(Path) {
    var q = std.PriorityQueue(Path, void, compare_path).init(allocator, {});
    var view = std.AutoHashMap(Pos, usize).init(allocator);
    var ret = std.ArrayList(Path).init(allocator);
    defer {
        var it = q.iterator();
        while (it.next()) |p| p.deinit();
        q.deinit();
        view.deinit();
    }
    errdefer {
        for (ret.items) |p| p.deinit();
        ret.deinit();
    }

    const scores = [_]usize{1, 1001, 1001};
    try q.add(Path.init(start, allocator));
    try view.put(.{.x = start.x, .y = start.y}, 0);

    while (q.removeOrNull()) |p| {
        if (p.pos.x == end.x and p.pos.y == end.y) {
            try ret.append(p);
            continue;
        }
        defer p.deinit();
        if (ret.items.len > 0 and p.score > ret.items[0].score) break;
        if (view.contains(p.pos) and view.get(p.pos).? < p.score) continue;

        const dirs = switch (p.dir) {
            '^' => "^><",
            'v' => "v<>",
            '>' => ">v^",
            '<' => "<^v",
            else => unreachable,
        };

        for (dirs, scores) |d, s| {
            const np = next_pos(.{.x = p.pos.x, .y = p.pos.y}, d);
            if (lines[np.y][np.x] == '#') continue;
            const r = try view.getOrPut(np);
            if (!r.found_existing or r.value_ptr.* >= p.score + s or r.value_ptr.* + 1000 == p.score + s) {
                var c = try p.clone();
                try c.path.append(d);
                c.pos = np;
                c.dir = d;
                c.score += s;
                r.value_ptr.* = c.score;
                try q.add(c);
            }
        }
    }

    return ret;
}

fn print_path(lines: std.ArrayList([]u8), path: []u8, start: Pos) !void {
    var cl = try lines.clone();
    defer cl.deinit();
    var pos = start;
    for (path) |c| {
        if (cl.items[pos.y][pos.x] == '.')
            cl.items[pos.y][pos.x] = c;
        pos = next_pos(pos, c);
    }
    for (cl.items) |l| print("{s}\n", .{l});
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(argv[1], allocator);
    defer utils.free_lines(lines);

    var start = Pos{};
    var end = Pos{};
    for (lines.items, 0..) |line, y| {
        // print("line : '{s}'\n", .{line});
        for (line, 0..) |c, x| {
            if (c == 'S')
                start = Pos{.x = x, .y = y};
            if (c == 'E')
                end = Pos{.x = x, .y = y};
        }
    }

    const paths = try dijk(lines.items, start, end, allocator);
    defer {
        for (paths.items) |p| p.deinit();
        paths.deinit();
    }
    // try print_path(lines, paths.items[0].path.items, start);
    print("Part 1: {}\n", .{ paths.items[0].score });

    var p2 = std.AutoHashMap(Pos, void).init(allocator);
    defer p2.deinit();
    for (paths.items) |p| {
        var pos = start;
        for (p.path.items) |c| {
            try p2.put(pos, {});
            pos = next_pos(pos, c);
        }
    }
    print("Part 2: {}\n", .{ p2.count() + 1 });
}
