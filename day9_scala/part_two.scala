import scala.collection.mutable.ListBuffer

object PartTwo {
    def main(args: Array[String]) = {
        val example_sum = calc_sum_of_extrapolated_values("example")
        assert(example_sum == 2)
        val input_sum = calc_sum_of_extrapolated_values("input")
        println("Part 2 Answer: " + input_sum)
    }

    def calc_sum_of_extrapolated_values(filename: String): Int = {
        var sum_of_extrapolated_values = 0

        for (line <- read_file(filename)) {
            val values = parse_line(line)
            var difference = calculate_differences(values)
            var all_zero = difference.forall(_ == 0)

            var differences = ListBuffer[List[Int]]()

            differences += difference

            while (!all_zero) {
                difference = calculate_differences(difference)
                differences += difference
                all_zero = difference.forall(_ == 0)
            }

            var extrapolated_value = 0

            for (d <- differences.reverse) {
                val first = d.head
                extrapolated_value = first - extrapolated_value
            }

            extrapolated_value = values.head - extrapolated_value
            sum_of_extrapolated_values += extrapolated_value
        }

        return sum_of_extrapolated_values
    }
    
    def read_file(filename: String): List[String] = {
        val bufferedSource = io.Source.fromFile(filename)
        val lines = (for (line <- bufferedSource.getLines()) yield line).toList
        bufferedSource.close
        return lines
    }

    def parse_line(line: String): List[Int] = {
        line.split(" ").map(_.toInt).toList
    }

    def calculate_differences(values: List[Int]): List[Int] = {
        val differences = ListBuffer[Int]()
        for (i <- 0 until values.length - 1) {
            differences += values(i + 1) - values(i)
        }
        return differences.toList
    }
}