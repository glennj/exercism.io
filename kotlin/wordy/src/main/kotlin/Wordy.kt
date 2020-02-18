import kotlin.math.pow

object Wordy {

    fun answer(input: String): Int =
        extractTokens(input).let { tokens ->
            // we should start with a number, and then alternate operators and numbers
            require(tokens.size % 2 == 1)
            var result = tokens[0].toIntOrNull(10) ?: throw IllegalArgumentException()
            for (i in 1 until tokens.size step 2) {
                val op = tokens[i]
                val num = tokens[i + 1].toIntOrNull(10) ?: throw IllegalArgumentException()
                when (op) {
                    "plus" -> result += num
                    "minus" -> result -= num
                    "multiplied" -> result *= num
                    "divided" -> result /= num
                    "pow" -> result = result.toDouble().pow(num).toInt()
                    else -> throw IllegalArgumentException()
                }
            }
            result
        }

    private fun extractTokens(input: String): List<String> =
        (Regex("What is (.+)\\?").matchEntire(input) ?: throw IllegalArgumentException())
            .groupValues[1]
            .replace(Regex("(multiplied|divided) by")) { it.groupValues[1] }
            .replace(Regex("raised to the (\\d+)(?:st|nd|rd|th) power")) { "pow ${it.groupValues[1]}" }
            .split(' ')
}
