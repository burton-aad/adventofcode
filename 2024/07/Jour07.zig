const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

fn read_line(line: []const u8, allocator: std.mem.Allocator) !std.ArrayList(usize) {
    var equ = std.ArrayList(usize).init(allocator);
    errdefer equ.deinit();
    var it = std.mem.splitAny(u8, line, ": ");
    while (it.next()) |e| {
        if (e.len > 0)
            try equ.append(try utils.int(usize, e));
    }
    return equ;
}

const opType = enum {
    add,
    mul,
    concat,

    fn run(self: opType, a: usize, b: usize) usize {
        return switch (self) {
            opType.add => a + b,
            opType.mul => a * b,
            opType.concat => a * std.math.pow(usize, 10, std.math.log10(b)+1) + b,
        };
    }
};

fn solve(equ: []const usize, ops: []const opType, allocator: std.mem.Allocator) !usize {
    var it = try utils.product(opType, ops, equ.len - 2, allocator);
    defer it.deinit();
    while (it.next()) |lops| {
        var v = equ[1];
        for (equ[2..], lops) |data, op| {
            v = op.run(v, data);
        }
        if (v == equ[0])
            return equ[0];
    }
    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();


    var buf: [1000]u8 = undefined;
    var sum: usize = 0;
    var sum2: usize = 0;
    while (try utils.readline(input.reader(), &buf)) |line| {
        const equ = try read_line(line, allocator);
        defer equ.deinit();
        // print("elems : '{any}'\n", .{equ.items});

        const ops = [_]opType{opType.add, opType.mul};
        var v = try solve(equ.items, &ops, allocator);
        if (v > 0) {
            // print("true 1 {}\n", .{v});
            sum += v;
            sum2 += v;
        }
        else {
            const ops2 = [_]opType{opType.add, opType.mul, opType.concat};
            v = try solve(equ.items, &ops2, allocator);
            // if (v > 0) print("true 2 {}\n", .{v});
            sum2 += v;
        }
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}
