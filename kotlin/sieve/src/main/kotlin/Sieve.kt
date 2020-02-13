fun Int.sqrt(): Int = kotlin.math.sqrt(this.toDouble()).toInt()

object Sieve {

    fun primesUpTo(upperBound: Int): List<Int> {
        // initially, all numbers are candidates
        val candidate = MutableList(size = upperBound + 1, init = { true })

        // 0 and 1 are not prime
        candidate[0] = false
        candidate[1] = false

        // mark all non-primes as not candidates
        for (i in 2..upperBound.sqrt()) {
            if (candidate[i])
                ((i * i)..upperBound)
                    .step(2 * if (i == 2) 1 else i)
                    .forEach { candidate[it] = false }
        }

        // the remaining candidates are prime
        return (2..upperBound).filter { candidate[it] }
    }
}
