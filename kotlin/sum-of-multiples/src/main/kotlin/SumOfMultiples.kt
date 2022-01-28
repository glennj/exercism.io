object SumOfMultiples {

    fun sum(factors: Set<Int>, limit: Int): Int {
        val multiples = mutableSetOf<Int>()
        for (f in factors.minus(0)) {
            for (m in 1..limit/f) {
                with(f * m) {
                    if (this < limit)
                        multiples.add(this)
                }
            }
        }
        return multiples.sum()
    }
}
