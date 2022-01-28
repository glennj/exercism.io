object Luhn {

    fun isValid(candidate: String): Boolean =
        with (candidate.filterNot(Char::isWhitespace)) {
            when {
                !this.all(Char::isDigit) -> false
                this.length <= 1 -> false
                else -> 0 == (
                        this.reversed()
                            .mapIndexed { i, c -> luhnDouble(i, c - '0') }
                            .sum()
                    ) % 10
            }
        }

    private fun luhnDouble(index: Int, digit: Int): Int =
        when {
            index.even() -> digit
            else -> with(digit * 2) { this - if (this > 9) 9 else 0 }

        }

    private fun Int.even(): Boolean = this % 2 == 0
}
