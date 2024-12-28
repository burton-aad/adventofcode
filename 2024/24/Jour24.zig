const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const Gate = enum { NONE, AND, OR, XOR, };

fn gateFromName(name: []const u8) Gate {
    if (std.mem.eql(u8, name, "AND")) { return Gate.AND; }
    else if (std.mem.eql(u8, name, "OR")) { return Gate.OR; }
    else if (std.mem.eql(u8, name, "XOR")) { return Gate.XOR; }
    else unreachable;
}

fn name_cmp(_: void, a: Wire.Name, b: Wire.Name) bool {
    for (a, b) |ca, cb| {
        if (ca < cb) {
            return true;
        }
        else if (ca > cb)
            return false;
    }
    return false;
}

const Wire = struct {
    const Name = [3]u8;

    name: Name,
    value: ?u1 = null,
    
    gate: Gate = Gate.NONE,
    lhs: Name = undefined,
    rhs: Name = undefined,

    const Self = @This();

    pub fn val(self: *Self, circuit: Circuit) u1 {
        return self.val_debug(circuit, null) catch {unreachable;};
    }

    pub fn val_debug(self: *Self, circuit: Circuit, debug: ?*std.ArrayList(Wire.Name)) !u1 {
        if (self.value == null) {
            if (debug) |d| { try d.append(self.name); }
            const lhs = try circuit.get(self.lhs).val_debug(circuit, debug);
            const rhs = try circuit.get(self.rhs).val_debug(circuit, debug);
            self.value = switch (self.gate) {
                Gate.AND => lhs & rhs,
                Gate.OR => lhs | rhs,
                Gate.XOR => lhs ^ rhs,
                else => unreachable,
            };
        }
        return self.value.?;
    }

    pub fn print(self: Self) void {
        if (self.gate == Gate.NONE) {
            utils.print("{s} -> {}\n", .{self.name, self.value.?});
        }
        else
            utils.print("{s} -> {s} {} {s}\n", .{self.name, self.lhs, self.gate, self.rhs});
    }

    pub fn reset(self: *Self) void {
        if (self.gate != Gate.NONE)
            self.value = null;
    }

    pub fn swap(self: *Self, o: *Wire) void {
        std.debug.assert(self.gate != Gate.NONE and o.gate != Gate.NONE);
        std.mem.swap(Name, &self.lhs, &o.lhs);
        std.mem.swap(Name, &self.rhs, &o.rhs);
        std.mem.swap(Gate, &self.gate, &o.gate);
    }
};

const Circuit = struct {
    map: std.AutoHashMap(Wire.Name, Wire),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        const map = std.AutoHashMap(Wire.Name, Wire).init(allocator);
        return .{ .map = map };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    pub fn print(self: Self) void {
        var it = self.map.valueIterator();
        while (it.next()) |v| v.print();
    }

    pub fn putValue(self: *Self, name: Wire.Name, value: u1) !void {
        try self.map.put(name, .{.name = name, .value = value});
    }

    pub fn putGate(self: *Self, name: Wire.Name, op: Gate, lhs: Wire.Name, rhs: Wire.Name) !void {
        if (lhs[0] == 'y' and rhs[0] == 'x' and lhs[1] == rhs[1] and lhs[2] == rhs[2]) {
            try self.map.put(name, .{.name = name, .gate = op, .lhs = rhs, .rhs = lhs});
        }
        else
            try self.map.put(name, .{.name = name, .gate = op, .lhs = lhs, .rhs = rhs});
    }

    pub fn get(self: Self, key: Wire.Name) *Wire {
        return self.map.getPtr(key).?;
    }

    pub fn reset(self: *Self) void {
        var it = self.map.valueIterator();
        while (it.next()) |v| { v.reset(); }
    }

    pub fn number(self: Self, prefix: u8) usize {
        var key = [_]u8{prefix, '0', '0'};
        var off : u6 = 0;
        var value: usize = 0;
        while (self.map.contains(key)) : ({
            off += 1;
            next_key(&key);
        }) {
            const v: usize = self.get(key).val(self);
            value |= v << off;
        }
        return value;
    }

    pub fn swap(self: Self, a: Wire.Name, b: Wire.Name) void {
        self.get(a).swap(self.get(b));
    }

    pub fn search(self: Self, lhs: Wire.Name, rhs: Wire.Name, gate: Gate) Wire.Name {
        var it = self.map.valueIterator();
        while (it.next()) |v| {
            if (v.gate == gate and std.mem.eql(u8, &v.lhs, &lhs) and std.mem.eql(u8, &v.rhs, &rhs))
                return v.name;
        }
        unreachable;
    }

    pub fn search_any(self: Self, lhs: Wire.Name, gate: Gate) Wire.Name {
        var it = self.map.valueIterator();
        while (it.next()) |v| {
            if (v.gate == gate and (std.mem.eql(u8, &v.lhs, &lhs) or std.mem.eql(u8, &v.rhs, &lhs)))
                return v.name;
        }
        unreachable;
    }
};

fn next_key(key: *Wire.Name) void {
    if (key[2] == '9') {
        key[1] += 1;
        key[2] = '0';
    }
    else
        key[2] += 1;
}

// Example operations
// x00 XOR y00 -> z00 ; sum0
// ------------------------------
// fps XOR rfg -> z01
// y01 XOR x01 -> fps ; sum1
// x00 AND y00 -> rfg ; car0
// ------------------------------
// nfs XOR mvw -> z02
// x02 XOR y02 -> nfs ; sum2
// fdn OR hnq -> mvw ; car1
// fps AND rfg -> fdn ; sum1 and car0
// x01 AND y01 -> hnq ; car1_only
// ------------------------------
// tgm XOR qtf -> z03
// x03 XOR y03 -> qtf ; sum3
// jqp OR hvw -> tgm ; car2
// mvw AND nfs -> jqp ; sum2 and car1
// x02 AND y02 -> hvw ; car2 only
fn search_sum(k: Wire.Name, circuit: Circuit) [5]Wire.Name {
    // Search correct calculation.
    // Order is important if the first operation is wrong.
    var r: [5]Wire.Name = undefined;
    r[4] = circuit.search(.{'x', k[1], k[2]}, .{'y', k[1], k[2]}, Gate.XOR);
    const pk = [_]u8{'x', if (k[2] == '0') k[1]-1 else k[1], if (k[2] == '0') '9' else k[2]-1};
    r[3] = circuit.search(.{'x', pk[1], pk[2]}, .{'y', pk[1], pk[2]}, Gate.AND);
    const prev_xor = circuit.search(.{'x', pk[1], pk[2]}, .{'y', pk[1], pk[2]}, Gate.XOR);
    r[2] = circuit.search_any(prev_xor, Gate.AND);
    r[1] = circuit.search_any(r[2], Gate.OR);
    r[0] = circuit.search_any(r[1], Gate.XOR);
    return r;
}

fn indexOf(slice: []const Wire.Name, v: Wire.Name) ?usize {
    for (slice[0..], 0..) |c, j| {
        if (std.mem.eql(u8, &c, &v)) return j;
    }
    return null;
}

fn search_swap(circuit: Circuit, allocator: std.mem.Allocator) !std.ArrayList(Wire.Name) {
    var xk = [_]u8{'x', '0', '0'};
    var zk = [_]u8{'z', '0', '0'};
    var i: usize = 0;
    var swap = std.ArrayList(Wire.Name).init(allocator);
    errdefer swap.deinit();
    var cur = std.ArrayList(Wire.Name).init(allocator);
    defer cur.deinit();

    while (circuit.map.contains(xk)) : ({ next_key(&xk); next_key(&zk); i += 1; }) {
        cur.clearRetainingCapacity();
        _ = try circuit.get(zk).val_debug(circuit, &cur);
        // No check for z00 and z01...
        if (i < 2) continue;

        // compare cur sum and expected sum and swap the different wire.
        const sum = search_sum(zk, circuit);
        for (cur.items) |w| {
            if (indexOf(&sum, w) == null) {
                for (sum) |sw| {
                    if (indexOf(cur.items, sw) == null) {
                        try swap.append(w);
                        try swap.append(sw);
                        circuit.swap(w, sw);
                        break;
                    }
                }
                break;
            }
        }
    }

    return swap;
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
    var circuit = Circuit.init(allocator);
    defer circuit.deinit();
    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        if (line.len == 0)
            break;
        try circuit.putValue(line[0..3].*, @intCast(line[5] - '0'));
    }

    while (try utils.readline(input.reader(), &buf)) |line| {
        // print("line : '{s}'\n", .{line});
        var it = std.mem.splitAny(u8, line, " ->");
        const lhs = it.next().?[0..3].*;
        const op = gateFromName(it.next().?);
        const rhs = it.next().?[0..3].*;
        _ = it.next();
        _ = it.next();
        _ = it.next();
        const name = it.next().?[0..3].*;
        try circuit.putGate(name, op, lhs, rhs);
    }
    // circuit.print();
    print("Part 1: {}\n", .{ circuit.number('z') });

    circuit.reset();
    const swap = try search_swap(circuit, allocator);
    defer swap.deinit();
    std.sort.block(Wire.Name, swap.items, {}, name_cmp);
    print("Part 2: ", .{});
    for (swap.items, 0..) |s, i| { if (i > 0) print(",",.{}); print("{s}",.{s}); }
    print("\n", .{});
}
