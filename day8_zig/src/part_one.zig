const std = @import("std");

const Tuple = std.meta.Tuple;
const print = std.debug.print;

const allocator = std.heap.page_allocator;

const str = []const u8;
const String = std.ArrayList(u8);

pub fn run() !void {
    const example_answer = try calc_steps_to_zzz("example");
    const example_answer2 = try calc_steps_to_zzz("example2");

    print("Example 1 Answer: {d}\n", .{example_answer});
    std.debug.assert(example_answer == 2);

    print("Example 2 Answer: {d}\n", .{example_answer2});
    std.debug.assert(example_answer2 == 6);

    const input_answer = try calc_steps_to_zzz("input");

    print("Part 1 Answer: {d}\n", .{input_answer});
}

pub fn calc_steps_to_zzz(filename: str) !u32 {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());

    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    var counter: u32 = 0;

    // first line
    var instructions: str = undefined;

    // rest of lines as hashmap of (key, Tuple(key, key))

    const Instruction = struct {
        l: str,
        r: str,
    };

    var nodes = std.StringHashMap(Instruction).init(allocator);
    defer nodes.deinit();

    var keys = std.ArrayList(str).init(allocator);
    defer keys.deinit();

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (counter == 0) {
            instructions = try allocator.dupe(u8, line);
        } else {
            if (line.len < 10) {
                continue;
            }

            var split = std.mem.split(u8, line, " = ");
            const key: str = split.next().?;
            const value: str = split.next().?;

            // that just feels so massively unnecessary - and so painfully difficult to debug when it goes wrong
            try keys.append(try allocator.dupe(u8, key));

            var value_split = std.mem.split(u8, value, ", ");

            var value_0: str = value_split.next().?;
            var value_1: str = value_split.next().?;

            // remove ( from value_0
            var value_0_split = std.mem.split(u8, value_0, "(");
            _ = value_0_split.next().?;
            value_0 = value_0_split.next().?;
            // remove ) from value_1
            var value_1_split = std.mem.split(u8, value_1, ")");
            value_1 = value_1_split.next().?;

            const value_tuple = Instruction{
                .l = try allocator.dupe(u8, value_0),
                .r = try allocator.dupe(u8, value_1),
            };

            _ = try nodes.put(try allocator.dupe(u8, key), value_tuple);
        }

        counter += 1;
    }

    var current_key: []const u8 = "AAA";
    var current_node = nodes.get(current_key).?;

    var is_finished = false;

    var steps_to_zzz: u32 = 0;

    while (!is_finished) {
        for (instructions) |instruction| {
            if (instruction == 'R') {
                current_key = current_node.r;
            } else if (instruction == 'L') {
                current_key = current_node.l;
            } else {
                continue;
            }

            steps_to_zzz += 1;

            current_node = nodes.get(current_key).?;

            if (std.mem.eql(u8, current_key, "ZZZ")) {
                is_finished = true;
                break;
            }
        }
    }

    return steps_to_zzz;
}
