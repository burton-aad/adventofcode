const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Dir = enum { up, down, right, left };

const Pos = struct {
    l: usize,
    c: usize,
    dir: Dir = Dir.up,

    const Self = @This();

    pub fn next(self: *Self, lines: []const []u8) ?Pos {
        switch (self.dir) {
            Dir.up => {
                if (self.l == 0) {
                    return null;
                }
                else if (lines[self.l-1][self.c] == '#') {
                    self.dir = Dir.right;
                }
                else
                    self.l -= 1;
            },

            Dir.right => {
                if (self.c == lines[0].len-1) {
                    return null;
                }
                else if (lines[self.l][self.c+1] == '#') {
                    self.dir = Dir.down;
                }
                else
                    self.c += 1;
            },

            Dir.down => {
                if (self.l == lines.len-1) {
                    return null;
                }
                else if (lines[self.l+1][self.c] == '#') {
                    self.dir = Dir.left;
                }
                else
                    self.l += 1;
            },

            Dir.left => {
                if (self.c == 0) {
                    return null;
                }
                else if (lines[self.l][self.c-1] == '#') {
                    self.dir = Dir.up;
                }
                else
                    self.c -= 1;
            },
        }

        return self.*;
    }
};

fn find_start(lines: []const []u8) !Pos {
    for (lines, 0..) |line, l| {
        for (line, 0..) |e, c| {
            if (e == '^')
                return .{ .l = l, .c = c };
        }
    }
    return error.NoStart;
}

fn print_map(map: []const []u8) void {
    for (map) |line| {
        print("{s}\n", .{line});
    }
    print("\n", .{});
}

fn search_loop(lines: [][]u8, start: Pos, blk: Pos, allocator: std.mem.Allocator) !bool {
    var pos = start;
    const old = lines[blk.l][blk.c];
    lines[blk.l][blk.c] = '#';
    defer lines[blk.l][blk.c] = old;
    var blocks = std.AutoHashMap(usize, void).init(allocator);
    defer blocks.deinit();

    var last_pos = pos;
    while (pos.next(lines)) |p| : (last_pos = pos) {
        if (last_pos.dir != p.dir) {
            const hash = @intFromEnum(p.dir) + p.c*1000 + p.l*1_000_000;
            if (blocks.contains(hash))
                return true;
            _ = try blocks.getOrPut(hash);
        }
    }
    return false;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InvalidParam; }
    var lines = try utils.read_lines(std.os.argv[1], allocator);
    defer utils.free_lines(lines);

    const start = try find_start(lines.items);
    var pos: Pos = start;
    // print("size {}x{}, start : '{any}'\n", .{lines.items.len, lines.items[0].len, start});
    lines.items[pos.l][pos.c] = 'X';
    
    var last_pos = pos;
    while (pos.next(lines.items)) |p| : (last_pos = pos) {
        if (lines.items[p.l][p.c] == '.') {
            if (p.dir == last_pos.dir
                    and try search_loop(lines.items, start, p, allocator)) {
                lines.items[p.l][p.c] = 'O';
            }
            else
                lines.items[p.l][p.c] = 'X';
        }
    }
    // print_map(lines.items);

    var block: usize = 0;
    var sum: usize = 0;
    for (lines.items) |line| {
        block += std.mem.count(u8, line, "O");
        sum += std.mem.count(u8, line, "X");
    }

    print("Part 1: {}\n", .{ sum + block });
    print("Part 2: {}\n", .{ block });
}

