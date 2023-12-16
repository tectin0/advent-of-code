const std = @import("std");
const part_one = @import("part_one.zig");
const part_two = @import("part_two.zig");

pub fn main() !void {
    try part_one.run();
    try part_two.run();
}
