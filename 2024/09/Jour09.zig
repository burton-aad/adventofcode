const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

const File = struct {
    id: usize,
    start: usize,
    size: usize,

    const Self = @This();

    pub fn checksum(self: Self) usize {
        const end = self.start + self.size - 1;
        if (self.start == 0) {
            return self.id * end * (end+1) / 2;
        }
        else
            return self.id * (end * (end+1) - (self.start-1) * self.start) / 2;
    }

    pub fn copy(self: Self) File {
        return .{.id = self.id, .start = self.pos, .size = self.size};
    }
};

const Mem = struct {
    files: std.ArrayList(File),
    free: std.ArrayList(File),

    const Self = @This();

    pub fn deinit(self: Self) void {
        self.files.deinit();
        self.free.deinit();
    }
};

fn make_mem(disk_map: []u8, allocator: std.mem.Allocator) !Mem {
    var files = std.ArrayList(File).init(allocator);
    var free = std.ArrayList(File).init(allocator);
    var free_space = true;
    var pos: usize = 0;
    var id: usize = 0;
    for (disk_map) |c| {
        free_space = !free_space;
        const size = c-'0';
        if (free_space)  {
            if (size > 0)
                try free.append(File{.id = 0, .start = pos, .size = size});
        }
        else {
            try files.append(File{.id = id, .start = pos, .size = size});
            id += 1;
        }
        pos += size;
    }
    return .{.files = files, .free = free};
}

fn make_mem_simple(disk_map: []u8, allocator: std.mem.Allocator) ![]usize {
    // old mapped version
    var mem_size: usize = 0;
    for (disk_map) |c| {
        mem_size += c - '0';
    }
    const mem = try allocator.alloc(usize, mem_size);

    var free_space = true;
    var memp: usize = 0;
    var id: usize = 0;
    for (disk_map) |c| {
        free_space = !free_space;
        if (!free_space) { id += 1; }
        for (0..c-'0') |_| {
            mem[memp] = if (free_space) 0 else id;
            memp += 1;
        }
    }
    return mem;
}

fn compact_mem(mem: *Mem) !void {
    const free = mem.free.items;
    var f: usize = 0;
    var b: usize = mem.files.items.len - 1;
    while (free[f].start < mem.files.items[b].start) {
        if (free[f].size >= mem.files.items[b].size) {
            mem.files.items[b].start = free[f].start;
            free[f].start += mem.files.items[b].size;
            free[f].size -= mem.files.items[b].size;
            b -= 1;
        } else {
            try mem.files.append(File{.id = mem.files.items[b].id,
                                      .start = free[f].start,
                                      .size = free[f].size});
            mem.files.items[b].size -= free[f].size;
            free[f].size = 0;
        }

        if (free[f].size == 0) {
            f += 1;
        }
    }
}

fn compact_files(mem: *Mem) !void {
    const free = mem.free.items;
    var b: usize = mem.files.items.len - 1;
    while (b > 0) : (b -= 1) {
        var f: usize = 0;
        while (f < free.len and free[f].start < mem.files.items[b].start) {
            if (free[f].size >= mem.files.items[b].size)
                break;
            f += 1;
        }
        else
            continue;

        mem.files.items[b].start = free[f].start;
        free[f].start += mem.files.items[b].size;
        free[f].size -= mem.files.items[b].size;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(std.os.argv[1], allocator);
    defer utils.free_lines(lines);

    var mem = try make_mem(lines.items[0], allocator);
    defer mem.deinit();
    try compact_mem(&mem);
    var sum: usize = 0;
    for (mem.files.items) |e| {
        sum += e.checksum();
    }
    print("Part 1: {}\n", .{ sum });

    var mem2 = try make_mem(lines.items[0], allocator);
    defer mem2.deinit();
    try compact_files(&mem2);
    sum = 0;
    for (mem2.files.items) |e| {
        sum += e.checksum();
    }
    print("Part 2: {}\n", .{ sum });
}
