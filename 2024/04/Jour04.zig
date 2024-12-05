const std = @import("std");
const utils = @import("utils.zig");
const print = utils.print;

fn check_target(lines: [][]u8, l: usize, c: usize) usize {
    const target = [_]u8{ 'X', 'M', 'A', 'S' };
    const rtarget = [_]u8{ 'S', 'A', 'M', 'X' };

    var count: usize = 0;
    const right = c <= lines[0].len-target.len;
    const down = l <= lines.len-target.len;
    const up = l >= target.len-1;

    // horizontal
    if (right and (std.mem.eql(u8, lines[l][c..c+4], &target)
                       or std.mem.eql(u8, lines[l][c..c+4], &rtarget)))
        count += 1;

    // vertical
    if (down and
            ((lines[l][c] == target[0] and lines[l+1][c] == target[1] and lines[l+2][c] == target[2] and lines[l+3][c] == target[3])
                 or (lines[l][c] == target[3] and lines[l+1][c] == target[2] and lines[l+2][c] == target[1] and lines[l+3][c] == target[0])))
        count += 1;

    // diag down
    if (down and right
            and ((lines[l][c] == target[0] and lines[l+1][c+1] == target[1] and lines[l+2][c+2] == target[2] and lines[l+3][c+3] == target[3])
                     or (lines[l][c] == target[3] and lines[l+1][c+1] == target[2] and lines[l+2][c+2] == target[1] and lines[l+3][c+3] == target[0])))
        count += 1;

    // diag up
    if (up and right
            and ((lines[l][c] == target[0] and lines[l-1][c+1] == target[1] and lines[l-2][c+2] == target[2] and lines[l-3][c+3] == target[3])
                     or (lines[l][c] == target[3] and lines[l-1][c+1] == target[2] and lines[l-2][c+2] == target[1] and lines[l-3][c+3] == target[0])))
        count += 1;

    return count;
}

fn check_target2(lines: [][]u8, l: usize, c: usize) usize {
    const target = [_]u8{ 'M', 'A', 'S' };
    const right = c <= lines[0].len-target.len;
    const down = l <= lines.len-target.len;

    if (down and right
            and ((lines[l][c] == target[0] and lines[l+1][c+1] == target[1] and lines[l+2][c+2] == target[2])
                     or (lines[l][c] == target[2] and lines[l+1][c+1] == target[1] and lines[l+2][c+2] == target[0]))
            and ((lines[l][c+2] == target[0] and lines[l+1][c+1] == target[1] and lines[l+2][c] == target[2])
                     or (lines[l][c+2] == target[2] and lines[l+1][c+1] == target[1] and lines[l+2][c] == target[0])))
        return 1;
    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) { return error.InavlidParam; }
    const lines = try utils.read_lines(std.os.argv[1], allocator);
    defer utils.free_lines(lines);

    var count: usize = 0;
    var count2: usize = 0;
    for (0..lines.items.len) |l| {
        // print("line : '{s}'\n", .{ lines.items[l] });
        for (0..lines.items[0].len) |c| {
            count += check_target(lines.items, l, c);
            count2 += check_target2(lines.items, l, c);
        }
    }

    print("Part 1: {}\n", .{ count });
    print("Part 2: {}\n", .{ count2 });
}

