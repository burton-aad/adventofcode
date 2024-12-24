const std = @import("std");

pub fn print(comptime format: []const u8, args: anytype) void
{
    std.io.getStdOut().writer().print(format, args) catch {};
}

pub fn readline(reader: anytype, buffer: []u8) !?[]u8 {
    return try reader.readUntilDelimiterOrEof(buffer, '\n');
}

pub fn Counter(comptime T: type, array: []T, alloc: std.mem.Allocator) !std.AutoHashMap(T, usize) {
    var map = std.AutoHashMap(T, usize).init(alloc);
    for (array) |e| {
        const r = try map.getOrPut(e);
        if (r.found_existing) r.value_ptr.* += 1 else r.value_ptr.* = 1;
    }
    return map;
}

pub fn diff(comptime T: type, a: T, b: T) T {
    return if (a > b) a-b else b-a;
}

pub fn int(comptime T: type, str: []const u8) !T
{
    if (std.mem.startsWith(u8, str, "0x"))
        return try std.fmt.parseInt(T, str, 16);
    return try std.fmt.parseInt(T, str, 10);
}

pub fn read_lines(path: [*:0]const u8, allocator: std.mem.Allocator) !std.ArrayList([]u8) {
    var lines = std.ArrayList([]u8).init(allocator);

    const input = try std.fs.cwd().openFileZ(path, .{});
    defer input.close();

    while (try input.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.pow(usize, 2, 24))) |line| {
        try lines.append(line);
    }

    return lines;
}

pub fn free_lines(lines: std.ArrayList([]u8)) void {
    for (lines.items) |l| {
        lines.allocator.free(l);
    }
    lines.deinit();
}

const combinatorType = enum {
    product,
    combine,
};

fn combinatoricIterator(comptime T: type, comptime ctype: combinatorType) type {
    return struct {
        const Self = @This();

        data: []const T,
        buf: []T,
        indexes: []usize,
        allocator: std.mem.Allocator,
        start: bool = false,

        pub fn init(data: []const T, repeat: usize, allocator: std.mem.Allocator) !Self {
            const buf = try allocator.alloc(T, repeat);
            errdefer allocator.free(buf);
            const indexes = try allocator.alloc(usize, repeat);
            errdefer allocator.free(indexes);
            switch (ctype) {
                combinatorType.product => @memset(indexes, 0),
                combinatorType.combine => for (indexes, 0..) |*ind, i| { ind.* = i; },
            }
            return .{
                .data = data,
                .buf = buf,
                .indexes = indexes,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            self.allocator.free(self.buf);
            self.allocator.free(self.indexes);
        }

        fn next_prod(self: *Self) bool {
            // return True for last element.
            var p = self.indexes.len - 1;
            self.indexes[p] += 1;
            while (p > 0 and self.indexes[p] == self.data.len) {
                self.indexes[p] = 0;
                self.indexes[p-1] += 1;
                p -= 1;
            }
            return (p==0 and self.indexes[p] == self.data.len);
        }

        fn next_comb(self: *Self) bool {
            // return True for last element.
            var p = self.indexes.len - 1;
            self.indexes[p] += 1;
            while (p > 0 and self.indexes[p] == self.data.len - self.indexes.len + p + 1) {
                self.indexes[p-1] += 1;
                p -= 1;
            }
            for (p+1..self.indexes.len) |i| {
                self.indexes[i] = self.indexes[i-1] + 1;
            }
            return (p==0 and self.indexes[p] == self.data.len - self.indexes.len + 1);
        }

        pub fn next(self: *Self) ?[]T {
            if (self.start) {
                if (
                    switch (ctype) {
                        combinatorType.product => self.next_prod(),
                        combinatorType.combine => self.next_comb(),
                    }
                )
                    return null;
            }
            else
                self.start = true;

            for (self.indexes, 0..) |i, b| {
                self.buf[b] = self.data[i];
            }
            return self.buf;
        }
    };
}

pub fn product(comptime T: type, data: []const T, repeat: usize, allocator: std.mem.Allocator) !combinatoricIterator(T, combinatorType.product) {
    return try combinatoricIterator(T, combinatorType.product).init(data, repeat, allocator);
}

pub fn combine(comptime T: type, data: []const T, repeat: usize, allocator: std.mem.Allocator) !combinatoricIterator(T, combinatorType.combine) {
    return try combinatoricIterator(T, combinatorType.combine).init(data, repeat, allocator);
}
