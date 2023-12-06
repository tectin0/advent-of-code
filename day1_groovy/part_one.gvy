def extract_first_and_last_digit_to_number(String filename) {
    input = new File(filename).readLines()

    sum = 0

    input.each { line ->
        v = []

        line.each {
            if (it.isNumber()) {
                v << it.toInteger()
            }
        }
        
        if (v.size() > 0) {
            sum += "${v[0]}${v[-1]}".toInteger()
        }
    }

    return sum
}

example = extract_first_and_last_digit_to_number("example_part_one")

assert example == 142

input = extract_first_and_last_digit_to_number("input")

println "Part 1 Answer: $input"


