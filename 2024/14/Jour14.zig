const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const SPACE_W = 101;
const SPACE_H = 103;

const Robot = struct {
    x: u64,
    y: u64,
    vx: i64,
    vy: i64,

    const Self = @This();

    pub fn parse_line(line: []u8) !Robot {
        var it = std.mem.splitAny(u8, line, "=, ");
        _ = it.next();
        const x = try utils.int(u64, it.next().?);
        const y = try utils.int(u64, it.next().?);
        _ = it.next();
        return .{ .x = x, .y = y, .vx = try utils.int(i64, it.next().?), .vy = try utils.int(i64, it.next().?) };
    }

    pub fn move(self: *Self) void {
        const p = move_robot(self.*, 1);
        self.x = p.x;
        self.y = p.y;
    }
};

fn move_l(p: u64, v: i64, t: u64, space: usize) u64 {
    if (v < 0) {
        const uv: u64 = @bitCast(-v);
        const rp = space - p - 1;
        const np = (rp + uv * t) % space;
        return space - np - 1;
    }
    else {
        const uv: u64 = @bitCast(v);
        return (p + uv * t) % space;
    }
}

fn move_robot(r: Robot, t: u64) struct {x: u64, y: u64} {
    return .{.x = move_l(r.x, r.vx, t,  SPACE_W), .y = move_l(r.y, r.vy, t, SPACE_H)};
}

fn print_robots(robots: []Robot) void {
    var space: [SPACE_H][SPACE_W]u8 = undefined;
    for (&space) |*line| { @memset(line, '.'); }
    for (robots) |r| { space[r.y][r.x] = '#'; }
    for (space) |line| { print("{s}\n", .{line}); }
}

fn search_tree(robots: []Robot) bool {
    var space: [SPACE_H][SPACE_W]bool = undefined;
    for (&space) |*line| { @memset(line, false); }
    for (robots) |r| { space[r.y][r.x] = true; }
    for (0..SPACE_H-2) |y| {
        for (2..SPACE_W-2) |x| {
            if (space[y][x]
                and space[y+1][x] and space[y+1][x-1] and space[y+1][x+1]
                and space[y+2][x] and space[y+2][x-1] and space[y+2][x-2] and space[y+2][x+1] and space[y+2][x+2]
            )
                return true;
        }
    }
    return false;
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
    const input = try std.fs.cwd().openFileZ(argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var q1: usize = 0;
    var q2: usize = 0;
    var q3: usize = 0;
    var q4: usize = 0;
    var robots = std.ArrayList(Robot).init(allocator);
    defer robots.deinit();
    while (try utils.readline(input.reader(), &buf)) |line| {
        const r = try Robot.parse_line(line);
        const p = move_robot(r, 100);
        try robots.append(r);
        if (p.x < SPACE_W / 2) {
            if (p.y < SPACE_H / 2) {
                q1 += 1;
            }
            else if (p.y > SPACE_H / 2) {
                q3 += 1;
            }

        }
        else if (p.x > SPACE_W / 2) {
            if (p.y < SPACE_H / 2) {
                q2 += 1;
            }
            else if (p.y > SPACE_H / 2) {
                q4 += 1;
            }
        }
    }

    //print("q: {}, {}, {}, {}\n", .{q1, q2, q3, q4});
    print("Part 1: {}\n", .{ q1 * q2 * q3 * q4 });

    var wline: [SPACE_H]usize = undefined; // line weights
    var i: usize = 1;
    while (true) : (i += 1) {
        for (robots.items) |*r| r.move();
        @memset(&wline, 0);
        for (robots.items) |r| wline[r.y] += 1;
        if (std.mem.max(usize, &wline) > 30 and search_tree(robots.items)) {
            print("Part 2: {}\n", .{ i });
            print_robots(robots.items);
            break;
        }
    }
}

