extends MainLoop

func _initialize():
    main()

func main():
    var total_points_example = calc_total_number_of_points("example")
    assert(total_points_example == 13)

    var total_points_input = calc_total_number_of_points("input")
    print("Part 1 Answer: " + str(total_points_input))

func calc_total_number_of_points(filename):
    var file = FileAccess.open(filename, FileAccess.READ)
    
    var total_points = 0

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

        var matches = []

        for i in range(winning_numbers_int.size()):
            if your_numbers_int.find(winning_numbers_int[i]) != -1:
                matches.append(winning_numbers_int[i])

        var number_of_matches = matches.size()

        var points = 0

        if number_of_matches > 0:
            points = (2 ** (number_of_matches - 1))

        total_points += points

    return total_points


