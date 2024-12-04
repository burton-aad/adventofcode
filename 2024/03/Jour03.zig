const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;


const State = enum {
    start,
    m, u, l, d, o, n, t,
    quote, no_arg, arg1, arg2,
};

fn ParseIterator(comptime T: type) type {
    return struct {
        buffer: []const T,
        index: usize,
        state: State,

        const Self = @This();

        fn start_state(e: u8) State {
            if (e == 'm')
                return State.m;
            if (e == 'd')
                return State.d;
            return State.start;
        }

        const Tag = enum { mul, do };
        pub fn next(self: *Self) ?union(Tag){ mul: struct { a: usize, b: usize }, do: bool } {
            self.state = State.start;
            var a: usize = 0;
            var b: usize = 0;
            var next_do = true;
            for (self.buffer[self.index..self.buffer.len]) |e| {
                // print("parse '{c}', state {}\n", .{e, self.state});
                self.index += 1;
                switch (self.state) {
                    State.start => { self.state = start_state(e); },
                    State.m => { self.state = if (e == 'u') State.u else start_state(e); },
                    State.u => { self.state = if (e == 'l') State.l else start_state(e); },
                    State.l => { a = 0; self.state = if (e == '(') State.arg1 else start_state(e);},
                    State.d => { self.state = if (e == 'o') State.o else start_state(e); },
                    State.o => { next_do = true; self.state = if (e == '(') State.no_arg else if (e == 'n') State.n else start_state(e); },
                    State.n => { self.state = if (e == '\'') State.quote else start_state(e); },
                    State.quote => { self.state = if (e == 't') State.t else start_state(e); },
                    State.t => { next_do = false; self.state = if (e == '(') State.no_arg else start_state(e); },
                    State.no_arg => {
                        if (e == ')')
                            return .{ .do = next_do };
                        self.state = start_state(e);
                    },
                    State.arg1 => {
                        if (e == ',') {
                            b = 0;
                            self.state = State.arg2;
                        }
                        else if (!std.ascii.isDigit(e)) {
                            self.state = start_state(e);
                        }
                        else
                            a = a*10 + (e - '0');
                    },
                    State.arg2 => {
                        if (e == ')') {
                            return .{ .mul = .{.a = a, .b = b} };
                        }
                        else if (!std.ascii.isDigit(e)) {
                            self.state = start_state(e);
                        }
                        else
                            b = b*10 + (e - '0');
                    },
                }
            }
            return null;
        }
    };
}

fn parse(buf: []const u8) ParseIterator(u8) {
    return .{
        .buffer = buf,
        .index = 0,
        .state = State.start,
    };
}

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const input = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer input.close();

    var buf: [10000]u8 = undefined;
    var sum: usize = 0;
    var sum2: usize = 0;
    var do = true;
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        var it = parse(line);
        while (it.next()) |v| {
            switch (v) {
                .mul => |s| {
                    sum += s.a * s.b;
                    if (do)
                        sum2 += s.a * s.b;
                },
                .do => |b| { do = b; }
            }
        }
    }

    print("Part 1: {}\n", .{ sum });
    print("Part 2: {}\n", .{ sum2 });
}

