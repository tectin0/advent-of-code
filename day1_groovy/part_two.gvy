numbers = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']

def extract_first_and_last_digit_to_number(String filename) {
    input = new File(filename).readLines()

    sum = 0

    input.each { line ->
        v = []

        buffer = []

        line.each { letter -> 
            if (letter.isNumber()) {
                buffer.clear()
                v << letter.toInteger()
            } else {
                buffer << letter

                numbers.each { number ->
                    if (buffer.join().contains(number)) {
                        index = numbers.indexOf(number)
                        v << (index + 1)

                        new_start_index = (buffer.size() - number.size() + 1)

                        buffer = buffer[new_start_index..-1]
                    }
                }
            }
        }

        if (v.size() > 0) {
            sum += "${v[0]}${v[-1]}".toInteger()
        }
    }

    return sum
}

example = extract_first_and_last_digit_to_number("example_part_two")

assert example == 281

input = extract_first_and_last_digit_to_number("input")

println "Part 2 Answer: $input"


