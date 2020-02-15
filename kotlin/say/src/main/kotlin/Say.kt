/* translation of this lovely recursive javascript solution
 * https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b
 */


class NumberSpeller {
    private val small = listOf(
            "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
            "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
    )

    private val tens = mapOf(
            20 to "twenty", 30 to "thirty", 40 to "forty", 50 to "fifty",
            60 to "sixty", 70 to "seventy", 80 to "eighty", 90 to "ninety"
    )

    private val units = mapOf(
            100 to "hundred", 1_000 to "thousand", 1_000_000 to "million", 1_000_000_000 to "billion"
    )

    fun say(input: Long): String = when {
            input < 0 -> throw IllegalArgumentException("input too small")
            input < 20 -> small[input.toInt()]
            input < 100 -> tens[input.toInt()] ?: "${say(input - (input % 10))}-${say(input % 10)}"
            input < 1_000 -> sayCompound(input, 100)
            input < 1_000_000 -> sayCompound(input, 1_000)
            input < 1_000_000_000 -> sayCompound(input, 1_000_000)
            input < 1_000_000_000_000 -> sayCompound(input, 1_000_000_000)
            else -> throw IllegalArgumentException("input too big")
        }

    private fun sayCompound(input: Long, base: Int): String =
            "${say(input / base)} ${units[base]}" +
             with(input % base) {
                 if (this > 0) " ${say(this)}" else ""
             }
}
