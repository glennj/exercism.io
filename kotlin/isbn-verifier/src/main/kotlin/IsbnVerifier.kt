class IsbnVerifier {
    fun isValid(number: String): Boolean =
        with (number.replace("-", "")) {
            matches(Regex("\\d{9}[\\dX]")) &&
            mapIndexed { i, c -> c.asInt() * (10 - i) }.sum() % 11 == 0
        }
}

fun Char.asInt(): Int = if (this.isDigit()) this - '0' else 10
