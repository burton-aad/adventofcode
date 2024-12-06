const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

fn print_hm(rules: anytype) void {
    var it = rules.iterator();
    while (it.next()) |e| {
        print("rule {} ->", .{ e.key_ptr.* });
        var inner = e.value_ptr.*.keyIterator();
        while (inner.next()) |v| { print(" {}", .{v.*}); }
        print("\n", .{});
    }
}

fn split_update(line: []u8, allocator: std.mem.Allocator) !std.ArrayList(usize) {
    var ret = std.ArrayList(usize).init(allocator);
    var it = std.mem.splitScalar(u8, line, ',');
    while (it.next()) |e| {
        try ret.append(try std.fmt.parseInt(usize, e, 10));
    }
    return ret;
}

fn check_rules(rules: anytype, e: usize, rest: []usize) ?usize {
    for (rest, 0..) |v, i| {
        if (rules.get(e)) |value| {
            if (value.contains(v))
                return i;
        }
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var rules = std.AutoHashMap(usize, std.AutoHashMap(usize, void)).init(allocator);
    defer {
        var it = rules.valueIterator();
        while (it.next()) |e| { e.*.deinit(); }
        rules.deinit();
    }
    while (try utils.readline(input.reader(), &buf)) |line| {
        if (line.len == 0)
            break;
        // print("rule : '{s}'\n", .{line});
        var it = std.mem.splitScalar(u8, line, '|');
        const first = try std.fmt.parseInt(usize, it.first(), 10);
        const last = try std.fmt.parseInt(usize, it.next().?, 10);
        const v = try rules.getOrPut(last);
        if (!v.found_existing) {
            v.value_ptr.* = std.AutoHashMap(usize, void).init(allocator);
        }
        try v.value_ptr.*.put(first, {});
    }
    // print_hm(rules);

    var sum: usize = 0;
    var sum2: usize = 0;
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("update : '{s}'\n", .{line});
        const upd = try split_update(line, allocator);
        defer upd.deinit();
        var valid = true;
        var i: usize = 0;
        while (i < upd.items.len) {
            const err = check_rules(rules, upd.items[i], upd.items[i+1..]);
            if (err != null) {
                valid = false;
                const tmp = upd.items[i];
                upd.items[i] = upd.items[i + 1 + err.?];
                upd.items[i + 1 + err.?] = tmp;
            }
            else
                i += 1;
        }
        if (valid) {
            sum += upd.items[upd.items.len / 2];
        }
        else
            sum2 += upd.items[upd.items.len / 2];
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}

