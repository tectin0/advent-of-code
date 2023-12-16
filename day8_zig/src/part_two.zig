const std = @import("std");

const Tuple = std.meta.Tuple;
const print = std.debug.print;

const allocator = std.heap.page_allocator;

const str = []const u8;
const String = std.ArrayList(u8);

pub fn run() !void {
    const example_answer = try calc_steps_to_zzz("example3");

    print("Example 3 Answer: {d}\n", .{example_answer});
    std.debug.assert(example_answer == 6);

    const input_answer = try calc_steps_to_zzz("input");

    // 10 000 000 000 too low
    print("Part 2 Answer: {d}\n", .{input_answer});
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

    var keys_that_end_in_a = std.ArrayList(str).init(allocator);
    defer keys_that_end_in_a.deinit();

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

            if (key[key.len - 1] == 'A') {
                try keys_that_end_in_a.append(try allocator.dupe(u8, key));
            }

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

    const current_keys: std.ArrayList(str) = keys_that_end_in_a;

    var keys_that_end_in_z: u32 = 0;
    var steps_to___z: u32 = 0;

    while (true) {
        for (instructions) |instruction| {
            keys_that_end_in_z = 0;

            if (instruction == 'R') {
                // iterate over current_keys
                var i: usize = 0;
                while (i < current_keys.items.len) : (i += 1) {
                    const key = current_keys.items[i];

                    const node = nodes.get(key).?;

                    current_keys.items[i] = node.r;

                    // check if current_keys last char is Z
                    if (current_keys.items[i][2] == 'Z') {
                        keys_that_end_in_z += 1;
                    }
                }
            } else if (instruction == 'L') {
                // iterate over current_keys
                var i: usize = 0;
                while (i < current_keys.items.len) : (i += 1) {
                    const key = current_keys.items[i];

                    const node = nodes.get(key).?;

                    current_keys.items[i] = node.l;

                    // check if keys last char is Z
                    if (current_keys.items[i][2] == 'Z') {
                        keys_that_end_in_z += 1;
                    }
                }
            } else {
                continue;
            }

            steps_to___z += 1;

            if (steps_to___z % 1000000 == 0) {
                print("Steps: {d}\n", .{steps_to___z});
            }

            if (keys_that_end_in_z == current_keys.items.len) {
                return steps_to___z;
            }
        }
    }

    return steps_to___z;
}
