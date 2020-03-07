import kotlin.math.sqrt

enum class Classification {
    DEFICIENT, PERFECT, ABUNDANT
}

val Int.factors: Set<Int>
    get() {
        require(this > 0)

        return (1..sqrt(this.toDouble()).toInt())
            .fold(emptySet<Int>()) { factors, i ->
                if (this % i == 0)
                    factors.plus(i).plus(this / i)
                else
                    factors
            }
            .let { it.minus(this) }
    }


fun classify(num: Int): Classification {
    require(num > 0)

    return with(num.factors.sum()) {
        when {
            this < num -> Classification.DEFICIENT
            this > num -> Classification.ABUNDANT
            else       -> Classification.PERFECT
        }
    }
}
