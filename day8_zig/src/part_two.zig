const std = @import("std");

const Tuple = std.meta.Tuple;
const print = std.debug.print;

const allocator = std.heap.page_allocator;

const str = []const u8;
const String = std.ArrayList(u8);

pub fn run() !void {
    const example_answer = try calc_steps_to___z("example3");

    print("Example 3 Answer: {d}\n", .{example_answer});
    std.debug.assert(example_answer == 6);

    const input_answer = try calc_steps_to___z("input");

    // brute force is not the answer
    // 3 days only at 280 763 000 000 steps with brute force
    // 14 265 111 103 729 in 75 ms with 'intelligent' approach
    // would have taken approx. 15242 days with brute force
    print("Part 2 Answer: {d}\n", .{input_answer});
}

pub fn calc_steps_to___z(filename: str) !u128 {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());

    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    var counter: u32 = 0;

    var instructions: str = undefined;

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
            if (line.len == 1) {
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

            var value_0_split = std.mem.split(u8, value_0, "(");
            _ = value_0_split.next().?;
            value_0 = value_0_split.next().?;

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

    var steps_to___z: u128 = 0;

    const KeySteps = struct {
        key: str,
        steps: u128,
    };

    var keys_to_keys = std.StringHashMap(KeySteps).init(allocator);
    defer keys_to_keys.deinit();

    var keys_not_done = std.ArrayList(str).init(allocator);
    defer keys_not_done.deinit();

    var i: usize = 0;
    while (i < keys.items.len) : (i += 1) {
        steps_to___z = 0;

        var key = keys.items[i];

        for (instructions) |instruction| {
            if (instruction == 'R') {
                const node = nodes.get(key).?;

                key = node.r;
            } else if (instruction == 'L') {
                const node = nodes.get(key).?;

                key = node.l;
            } else {
                continue;
            }

            steps_to___z += 1;

            if (key[2] == 'Z') {
                break;
            }
        }

        if ((key[2] != 'Z') and (!std.mem.eql(u8, keys.items[i], key))) {
            _ = try keys_not_done.append(try allocator.dupe(u8, keys.items[i]));
        }

        const key_steps = KeySteps{
            .key = key,
            .steps = steps_to___z,
        };

        _ = try keys_to_keys.put(try allocator.dupe(u8, keys.items[i]), key_steps);
    }

    var keys_not_done_buffer = std.ArrayList(str).init(allocator);

    while (keys_not_done.items.len > 0) {
        var k: usize = 0;

        while (k < keys_not_done.items.len) : (k += 1) {
            const key_not_done = keys_not_done.items[k];

            const key_to_key = keys_to_keys.get(key_not_done).?;

            const next_key = keys_to_keys.get(key_to_key.key).?;

            const next_steps = key_to_key.steps + next_key.steps;

            if (!std.mem.eql(u8, key_to_key.key, next_key.key)) {
                const next_key_steps = KeySteps{
                    .key = next_key.key,
                    .steps = next_steps,
                };

                _ = try keys_to_keys.put(try allocator.dupe(u8, key_not_done), next_key_steps);

                if (next_key.key[2] != 'Z') {
                    _ = try keys_not_done_buffer.append(try allocator.dupe(u8, key_not_done));
                }
            }
        }

        keys_not_done = keys_not_done_buffer;
        keys_not_done_buffer = std.ArrayList(str).init(allocator);
    }

    // least common multiple
    steps_to___z = 1;

    i = 0;
    while (i < keys_that_end_in_a.items.len) : (i += 1) {
        const key_that_ends_in_a = keys_that_end_in_a.items[i];
        const steps = keys_to_keys.get(key_that_ends_in_a).?.steps;
        steps_to___z = steps_to___z * steps / gcd(steps_to___z, steps);
    }

    return steps_to___z;
}

// https://rosettacode.org/wiki/Greatest_common_divisor
pub fn gcd(u: anytype, v: anytype) @TypeOf(u) {
    if (@typeInfo(@TypeOf(u)) != .Int) {
        @compileError("non-integer type used on gcd: " ++ @typeName(@TypeOf(u)));
    }
    if (@typeInfo(@TypeOf(v)) != .Int) {
        @compileError("non-integer type used on gcd: " ++ @typeName(@TypeOf(v)));
    }
    return if (v != 0) gcd(v, @mod(u, v)) else u;
}
