const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Pos = struct {
    x: usize,
    y: usize,
};

const Dir = enum {up, down, left, right};

const Region = struct {
    label: u8,
    area: std.ArrayList(Pos),
    perimeter: usize,
    sides: usize,

    const Self = @This();
    const tmp_mark = ';';
    const parsed_mark = '.';

    pub fn init(label: u8, allocator: std.mem.Allocator) Region {
        return .{
            .label = label,
            .area = std.ArrayList(Pos).init(allocator),
            .perimeter = 0,
            .sides = 0,
        };
    }

    pub fn deinit(self: Self) void {
        self.area.deinit();
    }

    pub fn in_area(self: Self, c: u8) bool { return c == self.label or c == Region.tmp_mark; }

    pub fn add_perimeter(self: *Self, lines: [][]u8, p: Pos, d: Dir) void {
        self.perimeter += 1;
        switch (d) {
            Dir.up => {
                if (p.x == 0 or !self.in_area(lines[p.y][p.x-1])) {
                    self.sides += 1;
                }
                else if (p.x > 0 and p.y > 0 and self.in_area(lines[p.y-1][p.x-1])) {
                    self.sides += 1;
                }
            },
            Dir.right => {
                if (p.y == 0 or !self.in_area(lines[p.y-1][p.x])) {
                    self.sides += 1;
                }
                else if (p.x < lines[0].len-1 and p.y > 0 and self.in_area(lines[p.y-1][p.x+1])) {
                    self.sides += 1;
                }
            },
            Dir.down => {
                if (p.x == lines[0].len-1 or !self.in_area(lines[p.y][p.x+1])) {
                    self.sides += 1;
                }
                else if (p.x < lines[0].len-1 and p.y < lines.len-1 and self.in_area(lines[p.y+1][p.x+1])) {
                    self.sides += 1;
                }
            },
            Dir.left => {
                if (p.y == lines.len-1 or !self.in_area(lines[p.y+1][p.x])) {
                    self.sides += 1;
                }
                else if (p.x > 0 and p.y < lines.len-1 and self.in_area(lines[p.y+1][p.x-1])) {
                    self.sides += 1;
                }
            },
        }
    }
};

const DirPos = struct {
    p: ?Pos,
    d: Dir,
};

fn neighbor4(lines: [][]u8, x: usize, y:usize) [4]DirPos {
    return .{
        .{.p = if (x > 0) Pos{.x = x-1, .y = y} else null, .d = Dir.left},
        .{.p = if (y > 0) Pos{.x = x, .y = y-1} else null, .d = Dir.up},
        .{.p = if (y < lines.len-1) Pos{.x = x, .y = y+1} else null, .d = Dir.down},
        .{.p = if (x < lines[0].len-1) Pos{.x = x+1, .y = y} else null, .d = Dir.right},
    };
}

fn parse_region(lines: [][]u8, x: usize, y: usize, allocator: std.mem.Allocator) !Region {
    var r = Region.init(lines[y][x], allocator);
    errdefer r.deinit();
    var q = std.ArrayList(Pos).init(allocator);
    defer q.deinit();
    try q.append(.{.x = x, .y = y});

    while (q.items.len > 0) {
        const pos = q.pop();
        if (lines[pos.y][pos.x] != r.label)
            continue;
        try r.area.append(pos);
        lines[pos.y][pos.x] = Region.tmp_mark;
        for (neighbor4(lines, pos.x, pos.y)) |dp| {
            if (dp.p) |p| {
                if (!r.in_area(lines[p.y][p.x])) {
                    r.add_perimeter(lines, pos, dp.d);
                }
                else
                    try q.append(p);
            }
            else
                r.add_perimeter(lines, pos, dp.d);
        }
    }
    for (r.area.items) |p| lines[p.y][p.x] = Region.parsed_mark;
    return r;
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

    var sum : usize = 0;
    var sum2: usize = 0;
    for (lines.items, 0..) |line, y| {
        for (line, 0..) |c, x| {
            if (c == '.') continue;
            const r = try parse_region(lines.items, x, y, allocator);
            sum += r.area.items.len * r.perimeter;
            sum2 += r.area.items.len * r.sides;
            r.deinit();
        }
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}

