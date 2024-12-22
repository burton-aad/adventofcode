const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Pos = struct { x: usize = 0, y: usize = 0 };

fn parse_map(lines: [][]u8, allocator: std.mem.Allocator) !std.AutoHashMap(Pos, usize) {
    var start = Pos{};
    outer: for (lines, 0..) |l, y| {
        for (l, 0..) |c, x| {
            if (c == 'S') {
                start = Pos{.x = x, .y = y};
                break :outer;
            }
        }
    }

    var p = start;
    var s: usize = 0;
    var map = std.AutoHashMap(Pos, usize).init(allocator);
    while (lines[p.y][p.x] != 'E') {
        try map.put(p, s);
        s += 1;
        if (lines[p.y][p.x+1] != '#' and !map.contains(.{.x = p.x+1, .y = p.y})) {
            p.x += 1;
        }
        else if (lines[p.y][p.x-1] != '#' and !map.contains(.{.x = p.x-1, .y = p.y})) {
            p.x -= 1;
        }
        else if (lines[p.y+1][p.x] != '#' and !map.contains(.{.x = p.x, .y = p.y+1})) {
            p.y += 1;
        }
        else if (lines[p.y-1][p.x] != '#' and !map.contains(.{.x = p.x, .y = p.y-1})) {
            p.y -= 1;
        }
        else 
            unreachable;
    }

    try map.put(p, s);
    return map;
} 

fn man_dist(s: Pos, e: Pos) usize {
    return utils.diff(usize, s.x, e.x) + utils.diff(usize, s.y, e.y);
}

fn cheat_count(lines: [][]u8, map: std.AutoHashMap(Pos, usize), time: usize, min_save: usize) usize {
    var count: usize = 0;
    for (lines[1..lines.len-1], 1..) |l, y| {
        for (l[1..l.len-1], 1..) |c, x| {
            if (c == '#') continue;
            const s = Pos{.x = x, .y = y};
            const startx = if (x > time) x-time else 1;
            const endx = if (x < l.len-|time) x+time+1 else l.len;
            const starty = if (y > time) y-time else 1;
            const endy = if (y < lines.len-|time) y+time+1 else lines.len;
            for (startx..endx) |ex| {
                for (starty..endy) |ey| {
                    const e = Pos{.x = ex, .y = ey};
                    const d = man_dist(s, e);
                    if (d > 0 and d <= time
                            and map.contains(e)
                            and map.get(s).? + min_save + d <= map.get(e).?) {
                        // print("{} -> {} save {}\n", .{s, e, map.get(e).? - map.get(s).? - 2});
                        count += 1;
                    }
                }
            }
        }
    }
    return count;
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(std.os.argv[1], allocator);
    defer utils.free_lines(lines);
    // for (lines.items) |l| print("line : '{s}'\n", .{l});

    const map = try parse_map(lines.items, allocator);
    defer @constCast(&map).deinit();
    // print("map {}\n", .{map.count()});

    print("Part 1: {}\n", .{ cheat_count(lines.items, map, 2, 100) });
    print("Part 2: {}\n", .{ cheat_count(lines.items, map, 20, 100) });
}

