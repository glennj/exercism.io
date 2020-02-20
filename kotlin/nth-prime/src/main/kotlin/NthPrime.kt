fun Int.sqrt(): Int = kotlin.math.sqrt(this.toDouble()).toInt()

fun Int.isPrime(): Boolean {
    if (this == 2) return true
    if (this % 2 == 0) return false
    (3..this.sqrt() step 2).forEach { if (this % it == 0) return false }
    return true
}

object Prime {

    fun nth(n: Int): Int {
        require(n > 0) {"There is no zeroth prime."}
  
        val primes = generateSequence(2) {
            when(it) {
                2 -> 3
                else -> {
                    var n = it
                    do { n += 2 } while (!n.isPrime())
                    n
                }
            }
        }.iterator()

        repeat(n - 1) { primes.next() }
        return primes.next()
    }
}
