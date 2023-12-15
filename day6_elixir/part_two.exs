example_match = [71530, 940200]
input_match = [63789468, 411127420471035]

calc_record_beating_ways = fn match ->
    [time, distance_record] = match
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

example_answer = calc_record_beating_ways.(example_match)

^example_answer = 71503

input_answer = calc_record_beating_ways.(input_match)

IO.puts "Part 2 Answer: #{input_answer}"
