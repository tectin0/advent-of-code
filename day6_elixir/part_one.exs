example_matches = [[7, 9], [15, 40], [30, 200]]
input_matches = [[63, 411], [78, 1274], [94, 2047], [68, 1035]]

calc_factor_record_beating_ways = fn matches ->
    factor = for [time, distance_record] <- matches do
        ways_to_beat_record = for t <- 0..time do
            speed = t
            t_left = time - t
            distance = speed * t_left
            if distance > distance_record do
                1
            else
                0
            end
        end

        Enum.reduce(ways_to_beat_record, 0, fn x, acc -> x + acc end)
    end

    factor = Enum.reduce(factor, 1, fn x, acc -> x * acc end)

    factor
end

example_answer = calc_factor_record_beating_ways.(example_matches)

^example_answer = 288

input_answer = calc_factor_record_beating_ways.(input_matches)

IO.puts "Part 1 Answer: #{input_answer}"
