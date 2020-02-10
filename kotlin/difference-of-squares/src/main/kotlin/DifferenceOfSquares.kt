import kotlin.math.pow

fun Int.pow(n: Int): Int = this.toFloat().pow(n).toInt()

class Squares(val n: Int) {

    fun sumOfSquares(): Int = (1..n).sumBy { it.pow(2) }

    fun squareOfSum(): Int = (1..n).sum().pow(2)

    fun difference(): Int = squareOfSum() - sumOfSquares()
}
