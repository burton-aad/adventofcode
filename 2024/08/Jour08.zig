const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Pos = struct {
    x: usize,
    y: usize,

    pub fn antinode(self: Pos, other: Pos, bounds: Pos, idx: usize) ?Pos {
        if ((2 + idx) * self.x < (1 + idx) * other.x)
            return null;
        const x = (2 + idx) * self.x - (1 + idx) * other.x;
        if (x >= bounds.x)
            return null;
        if ((2 + idx) * self.y < (1 + idx) * other.y)
            return null;
        const y = (2 + idx) * self.y - (1 + idx) * other.y;
        if (y >= bounds.y)
            return null;
        return .{ .x = x, .y = y };
    }
};

fn add_pos(node_map: *std.AutoHashMap(Pos, std.ArrayList(u8)), p: Pos, f: u8) !void {
    var it = try node_map.getOrPut(p);
    if (!it.found_existing) it.value_ptr.* = std.ArrayList(u8).init(node_map.allocator);
    try it.value_ptr.append(f);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(std.os.argv[1], allocator);
    defer utils.free_lines(lines);

    var antennas = std.AutoHashMap(u8, std.ArrayList(Pos)).init(allocator);
    defer {
        var it = antennas.valueIterator();
        while (it.next()) |v| { v.deinit(); }
        antennas.deinit();
    }
    const bounds: Pos = .{ .y = lines.items.len, .x = lines.items[0].len };
    for (lines.items, 0..) |line, y| {
        // print("line : '{s}'\n", .{line});
        for (line, 0..) |c, x| {
            if (c == '.')
                continue;
            var it = try antennas.getOrPut(c);
            if (!it.found_existing)
                it.value_ptr.* = std.ArrayList(Pos).init(allocator);
            try it.value_ptr.append(.{ .x=x, .y=y });
        }
    }

    var antinodes = std.AutoHashMap(Pos, std.ArrayList(u8)).init(allocator);
    defer {
        var it = antinodes.valueIterator();
        while (it.next()) |v| { v.deinit(); }
        antinodes.deinit();
    }
    var extra_antinodes = std.AutoHashMap(Pos, std.ArrayList(u8)).init(allocator);
    defer {
        var it = extra_antinodes.valueIterator();
        while (it.next()) |v| { v.deinit(); }
        extra_antinodes.deinit();
    }

    var antIt = antennas.iterator();
    while (antIt.next()) |entry| {
        // print("{c} -> {any}\n", .{entry.key_ptr.*, entry.value_ptr.items});
        var it = try utils.combine(Pos, entry.value_ptr.items, 2, allocator);
        defer it.deinit();
        while (it.next()) |arr| {
            var nc = true;
            var index: usize = 0;
            while (nc) : (index += 1) {
                nc = false;
                try add_pos(&extra_antinodes, arr[0], entry.key_ptr.*);
                if (arr[0].antinode(arr[1], bounds, index)) |p| {
                    if (index == 0)
                        try add_pos(&antinodes, p, entry.key_ptr.*);
                    try add_pos(&extra_antinodes, p, entry.key_ptr.*);
                    nc = true;
                }
                try add_pos(&extra_antinodes, arr[1], entry.key_ptr.*);
                if (arr[1].antinode(arr[0], bounds, index)) |p| {
                    if (index == 0)
                        try add_pos(&antinodes, p, entry.key_ptr.*);
                    try add_pos(&extra_antinodes, p, entry.key_ptr.*);
                    nc = true;
                }
            }
        }
    }

    print("Part 1: {}\n", .{ antinodes.count() });
    print("Part 2: {}\n", .{ extra_antinodes.count() });
}

