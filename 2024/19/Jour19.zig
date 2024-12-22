const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

// Part 1: grep -E "^($(head -n1 input | sed -e 's/, /|/g'))+$" input | wc

const Tree = struct {
    w : ?*Tree = null,
    u : ?*Tree = null,
    b : ?*Tree = null,
    r : ?*Tree = null,
    g : ?*Tree = null,
    out: bool = false,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Tree {
        return .{ . allocator = allocator };
    }

    pub fn deinit(self: Self) void {
        if (self.w) |t| { t.deinit(); self.allocator.destroy(t); }
        if (self.u) |t| { t.deinit(); self.allocator.destroy(t); }
        if (self.b) |t| { t.deinit(); self.allocator.destroy(t); }
        if (self.r) |t| { t.deinit(); self.allocator.destroy(t); }
        if (self.g) |t| { t.deinit(); self.allocator.destroy(t); }
    }

    fn get(self: Self, color: u8) ?*Tree {
        return switch (color) {
            'w' => self.w,
            'u' => self.u,
            'b' => self.b,
            'r' => self.r,
            'g' => self.g,
            else => unreachable,
        };
    }

    fn getOrPut(self: *Self, color: u8) !*Tree {
        switch (color) {
            'w' => {
                if (self.w == null) {
                    self.w = try self.allocator.create(Tree);
                    self.w.?.* = Tree.init(self.allocator);
                }
                return self.w.?;
            },
            'u' => {
                if (self.u == null) {
                    self.u = try self.allocator.create(Tree);
                    self.u.?.* = Tree.init(self.allocator);
                }
                return self.u.?;
            },
            'b' => {
                if (self.b == null) {
                    self.b = try self.allocator.create(Tree);
                    self.b.?.* = Tree.init(self.allocator);
                }
                return self.b.?;
            },
            'r' => {
                if (self.r == null) {
                    self.r = try self.allocator.create(Tree);
                    self.r.?.* = Tree.init(self.allocator);
                }
                return self.r.?;
            },
            'g' => {
                if (self.g == null) {
                    self.g = try self.allocator.create(Tree);
                    self.g.?.* = Tree.init(self.allocator);
                }
                return self.g.?;
            },
            else => unreachable,
        }
    }

    pub fn add_pattern(self: *Self, pattern: []const u8) !void {
        const ptr = try self.getOrPut(pattern[0]);
        if (pattern.len > 1) {
            try ptr.add_pattern(pattern[1..]);
        }
        else
            ptr.out = true;
    }

    fn rec_search(self: Self, base: Tree, design: []u8, memo: *std.StringHashMap(usize)) std.mem.Allocator.Error!usize {
        if (self.get(design[0])) |t| {
            if (design.len == 1)
                return if (t.out) 1 else 0;
            if (t.out) {
                return try base.search(design[1..], memo) + try t.rec_search(base, design[1..], memo);
            }
            else
                return try t.rec_search(base, design[1..], memo);
        }
        else
            return 0;
    }

    pub fn search(self: Self, design: []u8, memo: *std.StringHashMap(usize)) std.mem.Allocator.Error!usize {
        if (!memo.contains(design)) {
            try memo.put(design, try self.rec_search(self, design, memo));
        }
        return memo.get(design).?;
    }
};

fn make_tree(line: []u8, allocator: std.mem.Allocator) !Tree {
    var ret = Tree.init(allocator);
    var it = std.mem.splitSequence(u8, line, ", ");
    while (it.next()) |p| { try ret.add_pattern(p); }
    return ret;
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

    const tree = try make_tree(lines.items[0], allocator);
    defer tree.deinit();
    // print("final tree {any}\n", .{tree});

    var sum1: usize = 0;
    var sum2: usize = 0;
    var memo = std.StringHashMap(usize).init(allocator);
    defer memo.deinit();
    for (lines.items[2..]) |line| {
        // print("line : '{s}'\n", .{line});
        memo.clearRetainingCapacity();
        const n = try tree.search(line, &memo);
        sum1 += if (n > 0) 1 else 0;
        sum2 += n;
    }

    print("Part 1: {}\n", .{ sum1 });
    print("Part 2: {}\n", .{ sum2 });
}

