const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Pos = struct {
    x: usize = 0,
    y: usize = 0,
};

fn next_pos(p: Pos, m: u8) Pos {
    return switch (m) {
        '^' => .{.x = p.x, .y = p.y - 1},
        'v' => .{.x = p.x, .y = p.y + 1},
        '<' => .{.x = p.x - 1, .y = p.y},
        '>' => .{.x = p.x + 1, .y = p.y},
        else => unreachable,
    };
}

fn move_only_rec(warehouse: [][]u8, p: Pos, m: u8) ?Pos {
    const np = next_pos(p, m);
    const c = warehouse[np.y][np.x];
    if (c == '.' or ((c == 'O' or c == '[' or c == ']') and move_only_rec(warehouse, np, m) != null)) {
        warehouse[np.y][np.x] = warehouse[p.y][p.x];
        warehouse[p.y][p.x] = '.';
        return np;
    }
    else
        return null;
}

fn move_subrec(warehouse: [][]u8, p: Pos, m: u8, q: *std.AutoArrayHashMap(Pos, Pos)) !?Pos {
    const np = next_pos(p, m);
    if (q.contains(p))
        return np;
    const c = warehouse[np.y][np.x];
    const op = if (c == '[') Pos{.x = np.x+1, .y = np.y} else Pos{.x = np.x-1, .y = np.y};
    if (c == '.'
        or (c == 'O' and try move_subrec(warehouse, np, m, q) != null)
        or ((c == '[' or c == ']') and try move_subrec(warehouse, np, m, q) != null and try move_subrec(warehouse, op, m, q) != null)) {
        try q.put(p, np);
        return np;
    }
    else
        return null;
}

fn move(warehouse: [][]u8, p: Pos, m: u8) !?Pos {
    if (m == '>' or m == '<')
        return move_only_rec(warehouse, p, m);

    var buffer: [4096]u8 = undefined; // update size if necessary.
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var q = std.AutoArrayHashMap(Pos, Pos).init(allocator);
    defer q.deinit();
    const np = try move_subrec(warehouse, p, m, &q);
    if (np != null) {
        var it = q.iterator();
        while (it.next()) |e| {
            warehouse[e.value_ptr.y][e.value_ptr.x] = warehouse[e.key_ptr.y][e.key_ptr.x];
            warehouse[e.key_ptr.y][e.key_ptr.x] = '.';
        }
    }
    return np;
}

fn gps(warehouse: [][]u8, search: u8) usize {
    var sum: usize = 0;
    for (warehouse, 0..) |l, y| {
        for (l, 0..) |c, x| {
            if (c == search)
                sum += 100 * y + x;
        }
    }
    return sum;
}

pub fn main() !void {
    //var timer = try std.time.Timer.start();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(argv[1], .{});
    defer input.close();

    var buf: [1024]u8 = undefined;
    var warehouse = std.ArrayList([]u8).init(allocator);
    var warehouse2 = std.ArrayList([]u8).init(allocator);
    defer {
        for (warehouse.items) |l| allocator.free(l);
        warehouse.deinit();
        for (warehouse2.items) |l| allocator.free(l);
        warehouse2.deinit();
    }
    var robot = Pos{};
    var robot2 = Pos{};
    var y: usize = 0;
    while (try utils.readline(input.reader(), &buf)) |line| : (y += 1) {
        if (line.len == 0)
            break;
        var l = try allocator.alloc(u8, line.len);
        var l2 = try allocator.alloc(u8, line.len * 2);
        for (line, 0..) |c, x| {
            l[x] = c;
            switch (c) {
                '#' => { l2[2*x] = '#'; l2[2*x+1] = '#'; },
                'O' => { l2[2*x] = '['; l2[2*x+1] = ']'; },
                '.' => { l2[2*x] = '.'; l2[2*x+1] = '.'; },
                '@' => {
                    l2[2*x] = '@';
                    l2[2*x+1] = '.';
                    robot.x = x;
                    robot.y = y;
                    robot2.x = 2*x;
                    robot2.y = y;
                },
                else => unreachable,
            }
        }
        try warehouse.append(l);
        try warehouse2.append(l2);
    }

    // for (warehouse2.items) |l| print("{s}\n", .{l});
    // print("robot {}\n", .{robot2});

    while (try utils.readline(input.reader(), &buf)) |line| {
        for (line) |c| {
            robot = try move(warehouse.items, robot, c) orelse robot;
            robot2 = try move(warehouse2.items, robot2, c) orelse robot2;
            //print("\nMove {c}:\n", .{c});
            //for (warehouse2.items) |l| print("{s}\n", .{l});
            //print("robot {}\n", .{robot2});
        }
    }
    // for (warehouse2.items) |l| print("{s}\n", .{l});

    print("Part 1: {}\n", .{ gps(warehouse.items, 'O') });
    print("Part 2: {}\n", .{ gps(warehouse2.items, '[') });

    //print("Time : {} ms\n", .{ timer.read() / 1000000 });
}
