const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Opcode = enum(u3) {
    adv = 0,
    bxl,
    bst,
    jnz,
    bxc,
    out,
    bdv,
    cdv,
};

const Prog = struct {
    A: usize,
    B: usize,
    C: usize,
    insts: []u3,
    pc: usize,
    allocator: std.mem.Allocator,
    out: std.ArrayList(usize),

    const Self = @This();

    pub fn init(prog: []const u8, allocator: std.mem.Allocator) !Prog {
        const prog_size = std.mem.count(u8, prog, ",") + 1;
        var insts = try allocator.alloc(u3, prog_size);
        var it = std.mem.splitSequence(u8, prog, ",");
        var i: usize = 0;
        while (it.next()) |inst| : (i+=1) { insts[i] = try utils.int(u3, inst); }
        return .{
            .A = 0, .B = 0, .C = 0, .pc = 0,
            .out = std.ArrayList(usize).init(allocator),
            .insts = insts, .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.out.deinit();
        self.allocator.free(self.insts);
    }

    pub fn reset(self: *Self, A: usize) void {
        self.A = A;
        self.B = 0;
        self.C = 0;
        self.pc = 0;
        self.out.clearRetainingCapacity();
    }

    pub fn print_regs(self: Self) void {
        print("Registers: A {}, B {}, C {}. pc {}\n", .{self.A, self.B, self.C, self.pc});
    }

    pub fn print_prog(self: Self) void {
        self.print_regs();
        var i: usize = 0;
        while (i < self.insts.len-1) : (i+=2) {
            print("{s}({}) {}\n", .{@tagName(@as(Opcode, @enumFromInt(self.insts[i]))), self.insts[i], self.insts[i+1]});
        }
    }

    pub fn combo(self: Self, operand: u3) usize {
        return switch (operand) {
            0...3 => operand,
            4 => self.A,
            5 => self.B,
            6 => self.C,
            7 => unreachable,
        };
    }

    fn run_one(self: *Self) ?usize {
        const pc = self.pc;
        const operand = self.insts[pc+1];
        self.pc += 2;
        switch (@as(Opcode, @enumFromInt(self.insts[pc]))) {
            Opcode.adv => self.A = self.A / std.math.pow(usize, 2, self.combo(operand)),
            Opcode.bxl => self.B = self.B ^ operand,
            Opcode.bst => self.B = self.combo(operand) % 8,
            Opcode.jnz => if (self.A != 0) { self.pc = operand; },
            Opcode.bxc => self.B = self.B ^ self.C,
            Opcode.out => return self.combo(operand) % 8,
            Opcode.bdv => self.B = self.A / std.math.pow(usize, 2, self.combo(operand)),
            Opcode.cdv => self.C = self.A / std.math.pow(usize, 2, self.combo(operand)),
        }
        return null;
    }

    pub fn search_out(self: *Self) usize {
        while (self.pc < self.insts.len - 1) {
            if (self.run_one()) |v| return v;
        }
        unreachable;
    }

    pub fn run(self: *Self) !void {
        while (self.pc < self.insts.len - 1) {
            if (self.run_one()) |v| {
                try self.out.append(v);
            }
            // self.print_prog();
        }
        // print("\nEnd\n", .{});
    }
};

fn parse_prog(input_path: [:0]u8, allocator: std.mem.Allocator) !Prog {
    const input = try std.fs.cwd().openFileZ(input_path, .{});
    defer input.close();
    var buf: [1000]u8 = undefined;

    var line = (try utils.readline(input.reader(), &buf)).?;
    var it = std.mem.splitSequence(u8, line, " ");
    _ = it.next();
    _ = it.next();
    const A = try utils.int(usize, it.next().?);

    line = (try utils.readline(input.reader(), &buf)).?;
    it = std.mem.splitSequence(u8, line, " ");
    _ = it.next();
    _ = it.next();
    const B = try utils.int(usize, it.next().?);

    line = (try utils.readline(input.reader(), &buf)).?;
    it = std.mem.splitSequence(u8, line, " ");
    _ = it.next();
    _ = it.next();
    const C = try utils.int(usize, it.next().?);

    _ = try utils.readline(input.reader(), &buf);
    line = (try utils.readline(input.reader(), &buf)).?;
    it = std.mem.splitSequence(u8, line, " ");
    _ = it.next();
    var p = try Prog.init(it.next().?, allocator);
    p.A = A;
    p.B = B;
    p.C = C;

    return p;
}

fn search_adv(p: Prog) usize {
    var i: usize = 0;
    while (i < p.insts.len - 1) : (i += 2) {
        if (@as(Opcode, @enumFromInt(p.insts[i])) == Opcode.adv)
            return std.math.pow(usize, 2, p.combo(p.insts[i+1]));
    }
    return 0;
}

fn search_quine(p: *Prog, adv: usize) usize {
    var start: usize = 1;
    var end: usize = adv;
    var i: usize = 0;
    while (i < p.insts.len) : ({
        i += 1;
        start *= adv;
        end *= adv;
    }) {
        const t = p.insts[p.insts.len-i-1];
        while (start < end) : (start += 1) {
            p.reset(start);
            if (p.search_out() == t) {
                // print("find t {} with s {}\n", .{t, start});
                break;
            }
        }
        else
            unreachable;
    }
    return start / 8;
}

fn print_tab(prefix: []u8, t: []usize) void {
    print("{}: ", .{ prefix });
    for (t, 0..) |c, i| {if (i>0) print(",", .{}); print("{}", .{c});}
    print("\n", .{});
}

pub fn main() !void {
    // var timer = try std.time.Timer.start();
    // defer print("Time : {} ms\n", .{ timer.read() / 1000000 });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    if (argv.len < 2) { return error.InavlidParam; }

    var p = try parse_prog(argv[1], allocator);
    defer p.deinit();
    // p.print_prog();
    try p.run();
    print("Part 1: ", .{});
    for (p.out.items, 0..) |c, i| {if (i>0) print(",", .{}); print("{}", .{c});}
    print("\n", .{});

    const adv = search_adv(p);
    // print("adv {}\n", .{adv});
    const quine = search_quine(&p, adv);
    p.reset(quine);
    try p.run();
    print("Part 2: {} -> ", .{ quine });
    for (p.out.items, 0..) |c, i| {if (i>0) print(",", .{}); print("{}", .{c});}
    print("\n", .{});
}
