class Series(digitString: String) {
    init {
        require(digitString.all(Char::isDigit))
    }

    private val digits = digitString.toCharArray().map { it - '0' }

    fun getLargestProduct(span: Int): Long {
        require(0 <= span && span <= digits.size)

        return when {
            span == 0 -> 1
            digits.isEmpty() -> 1
            else -> digits.windowed(span, step = 1) { it.product() }.max()!!
        }
    }
}

fun List<Int>.product(): Long = this.fold(1L) { prod, n -> prod * n }
