class IsbnVerifier {

    private fun Char.asInt(): Int = if (this.isDigit()) this - '0' else 10

    fun isValid(number: String): Boolean =
        with (number.replace("-", "")) {
            when (Regex("\\d{9}[\\dX]").matchEntire(this)) {
                null -> false
                else -> {
                    this.reversed()
                        .mapIndexed { i, c -> (i + 1) * c.asInt() }
                        .sum() % 11 == 0
                }
            }
        }
}
