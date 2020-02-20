object PrimeFactorCalculator {

    fun primeFactors(int: Int): List<Int> =
        primeFactors(int.toLong()).map(Long::toInt)


    tailrec fun primeFactors(
            n: Long,
            f: Long = 2L,
            factors: List<Long> = emptyList()
    ): List<Long> =
        when {
            n == 1L -> factors
            f * f > n -> factors + n
            n % f == 0L -> primeFactors(n / f, f, factors + f)
            else -> {
                val next = if (f == 2L) 3L else f + 2
                primeFactors(n, next, factors)
            }
        }
}
