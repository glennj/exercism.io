class BaseConverter(val fromBase: Int, val digits: IntArray) {
    init {
        require(fromBase > 1) { "Bases must be at least 2." }
        require(digits.isNotEmpty()) { "You must supply at least one digit." }
        require(!(digits.size > 1 && digits[0] == 0)) { "Digits may not contain leading zeros." }
        require(digits.all { it >= 0 }) { "Digits may not be negative." }
        require(digits.all { it < fromBase }) { "All digits must be strictly less than the base." }
    }

    private val decimal = digits.reduce { sum, d -> sum * fromBase + d }

    fun convertToBase(newBase: Int): IntArray {
        require(newBase > 1) { "Bases must be at least 2." }

        if (decimal < newBase) return intArrayOf(decimal)

        var d = decimal
        val newDigits = mutableListOf<Int>()
        while (d > 0) {
            newDigits.add(0, d % newBase)
            d /= newBase
        }
        return newDigits.toIntArray()
    }
}
