const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

fn indexOfC(slice: []const Computer, value: Computer) ?usize {
    for (slice, 0..) |c, j| {
        if (c[0] == value[0] and c[1] == value[1]) return j;
    }
    return null;
}

fn indexOfAnyC(slice: []const Computer, values: []const Computer) ?usize {
    for (slice, 0..) |c, i| {
        for (values) |value| {
            if (c[0] == value[0] and c[1] == value[1]) return i;
        }
    }
    return null;
}

fn search_3set(links: Links, allocator: std.mem.Allocator) !std.ArrayList([3]Computer) {
    var r = std.ArrayList([3]Computer).init(allocator);
    errdefer r.deinit();
    var it_l = links.keyIterator();
    var view = std.ArrayList(Computer).init(allocator);
    defer view.deinit();
    while (it_l.next()) |k| {
        var it = try utils.combine(Computer, links.get(k.*).?.items, 2, allocator);
        defer it.deinit();
        while (it.next()) |e| {
            if (indexOfAnyC(view.items, e) != null)
                continue;
            if (indexOfC(links.get(e[0]).?.items, e[1]) != null)
                try r.append(.{k.*, e[0], e[1]});
        }
        try view.append(k.*);
    }
    return r;
}

const SetIterator = struct {
    const Self = @This();

    buf: []Computer,
    links: Links,
    it_l: Links.KeyIterator,
    it: utils.combinatoricIterator(Computer, utils.combinatorType.combine),
    view: std.ArrayList(Computer),
    allocator: std.mem.Allocator,

    pub fn init(links: Links, size: usize, allocator: std.mem.Allocator) !Self {
        const buf = try allocator.alloc(Computer, size);
        errdefer allocator.free(buf);
        var view = try std.ArrayList(Computer).initCapacity(allocator, links.count());
        errdefer view.deinit();
        var it_l = links.keyIterator();
        const pk = it_l.next();
        if (pk == null)
            return error.EmptyLinks;
        buf[0] = pk.?.*;
        view.appendAssumeCapacity(pk.?.*);
        return .{
            .buf = buf,
            .links = links,
            .it_l = it_l,
            .it = try utils.combine(Computer, links.get(pk.?.*).?.items, size-1, allocator),
            .view = view,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.it.deinit();
        self.view.deinit();
        self.allocator.free(self.buf);
    }

    pub fn next(self: *Self) !?[]Computer {
        var buffer: [32]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&buffer);
        const allocator = fba.allocator();
        var it = try utils.combine(Computer, self.buf, 2, allocator);

        while (true) {
            while (self.it.next()) |e| {
                if (indexOfAnyC(self.view.items, e) != null)
                    continue;
                it.resetData(e);
                while (it.next()) |pair| {
                    if (indexOfC(self.links.get(pair[0]).?.items, pair[1]) == null)
                        break;
                }
                else {
                    for (e, 1..) |v, i| self.buf[i] = v;
                    return self.buf;
                }
            }
            if (self.it_l.next()) |k| {
                self.it.resetData(self.links.get(k.*).?.items);
                self.buf[0] = k.*;
                self.view.appendAssumeCapacity(k.*);
            }
            else
                return null;
        }
    }
};

const Computer = [2]u8;
const Links = std.AutoHashMap(Computer, std.ArrayList(Computer));

fn orderC(_: void, lhs: Computer, rhs: Computer) bool {
    return (lhs[0] < rhs[0]) or (lhs[0] == rhs[0] and lhs[1] < rhs[1]);
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [1000]u8 = undefined;
    var links = Links.init(allocator);
    defer {
        var it = links.valueIterator();
        while (it.next()) |v| v.deinit();
        links.deinit();
    }
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        const l = line[0..2];
        const r = line[3..5];
        const e = try links.getOrPut(l.*);
        if (!e.found_existing) e.value_ptr.* = std.ArrayList(Computer).init(allocator);
        try e.value_ptr.append(r.*);
        const f = try links.getOrPut(r.*);
        if (!f.found_existing) f.value_ptr.* = std.ArrayList(Computer).init(allocator);
        try f.value_ptr.append(l.*);
    }

    // {
    //     var it = links.iterator();
    //     while (it.next()) |e| print("{s} -> {s}\n", .{e.key_ptr.*, e.value_ptr.items});
    // }
    
    {
        var sets = try SetIterator.init(links, 3, allocator);
        defer sets.deinit();
        var sum: usize = 0;
        while (try sets.next()) |set| {
            if (set[0][0] == 't' or set[1][0] == 't' or set[2][0] == 't')
                sum += 1;
        }
        print("Part 1: {}\n", .{ sum });
    }

    var password: [30]Computer = undefined;
    var i: usize = 4;
    while (i < password.len) : (i += 1) {
        var sets = try SetIterator.init(links, i, allocator);
        defer sets.deinit();
        if (try sets.next()) |set| {
            @memcpy(password[0..i], set);
            // print("{} -> {s}\n", .{ i, password[0..i]});
        }
        else
            break;
    }
    std.sort.block(Computer, password[0..i-1], {}, orderC);
    print("Part 2: ", .{});
    for (0..i-1) |j| {if (j > 0) print(",", .{}); print("{s}", .{ password[j] });}
    print("\n", .{});
}

