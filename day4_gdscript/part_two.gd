extends MainLoop

func _initialize():
    main()

func main():
    var total_scratch_cards_example = calc_total_scratch_cards("example")
    assert(total_scratch_cards_example == 30)

    var total_scratch_cards_input = calc_total_scratch_cards("input")
    print("Part 2 Answer: " + str(total_scratch_cards_input))

func calc_total_scratch_cards(filename):
    var file = FileAccess.open(filename, FileAccess.READ)

    var total_scratch_cards = 0

    var matches = []

    while not file.eof_reached():
        var line = file.get_line().split(":")
        line = line[1].split("|")

        var winning_numbers = line[0].split(" ")
        var your_numbers = line[1].split(" ")

        var winning_numbers_int = []
        var your_numbers_int = []

        for i in range(winning_numbers.size()):
            if winning_numbers[i] == "":
                continue

            var number = int(winning_numbers[i].strip_edges(true, true))
            winning_numbers_int.append(number)

        for i in range(your_numbers.size()):
            if your_numbers[i] == "":
                continue

            var number = int(your_numbers[i].strip_edges(true, true))
            your_numbers_int.append(number)

        var matches_current_card = []

        for i in range(winning_numbers_int.size()):
            if your_numbers_int.find(winning_numbers_int[i]) != -1:
                matches_current_card.append(winning_numbers_int[i])

        var number_of_matches = matches_current_card.size()

        matches.append(number_of_matches)

    var scratch_cards_count = []
    for i in range(matches.size()):
        scratch_cards_count.append(1)

    for i in range(scratch_cards_count.size()):
        var current_count = scratch_cards_count[i]
        var current_matches = matches[i]

        for j in range(current_matches + 1):
            if j == 0:
                continue

            if i + j < scratch_cards_count.size():
                scratch_cards_count[i + j] += current_count

        total_scratch_cards += scratch_cards_count[i]

    return total_scratch_cards


