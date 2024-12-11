const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const hikTrail = struct {
    height: usize,
    score: ?usize = null,
};

const Map = std.ArrayList(std.ArrayList(hikTrail));

pub fn score(topo_map: Map, i: usize, j: usize, access: *std.AutoHashMap([2]usize, void)) !usize {
    var p = topo_map.items[i].items[j];
    if (p.score == null) {
        if (p.height == 9) {
            try access.put(.{i, j}, {});
            p.score = 1;
            return p.score.?;
        }

        var s: usize = 0;
        if (i > 0 and topo_map.items[i-1].items[j].height == p.height+1)
            s += try score(topo_map, i-1, j, access);
        if (i < topo_map.items.len - 1 and topo_map.items[i+1].items[j].height == p.height+1)
            s += try score(topo_map, i+1, j, access);
        if (j > 0 and topo_map.items[i].items[j-1].height == p.height+1)
            s += try score(topo_map, i, j-1, access);
        if (j < topo_map.items[0].items.len - 1 and topo_map.items[i].items[j+1].height == p.height+1)
            s += try score(topo_map, i, j+1, access);
        p.score = s;
    }
    return p.score.?;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) {
        return error.InvalidParam;
    }
    const input = try std.fs.cwd().openFileZ(argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var topo_map = Map.init(allocator);
    defer {
        for (topo_map.items) |t| { t.deinit(); }
        topo_map.deinit();
    }
    while (try utils.readline(input.reader(), &buf)) |line| {
        //print("line : '{s}'\n", .{line});
        var t = std.ArrayList(hikTrail).init(allocator);
        errdefer t.deinit();
        for (line) |c| { try t.append( hikTrail{ .height = c - '0' } ); }
        try topo_map.append(t);
    }

    var sum: usize = 0;
    var sum2: usize = 0;
    var access = std.AutoHashMap([2]usize, void).init(allocator);
    defer access.deinit();
    for (topo_map.items, 0..) |it, i| {
        for (it.items, 0..) |c, j| {
            if (c.height == 0) {
                access.clearRetainingCapacity();
                sum2 += try score(topo_map, i, j, &access);
                sum += access.count();
            }
        }
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}
