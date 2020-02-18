import kotlin.math.ceil
import kotlin.math.sqrt

object CryptoSquare {

    fun ciphertext(plaintext: String): String {
        var letters = plaintext
                .filter(Char::isLetterOrDigit)
                .map(Char::toLowerCase)
                .joinToString("")

        if (letters.isEmpty()) return ""

        val size = ceil(sqrt(letters.length.toDouble())).toInt()

        if (letters.length % size != 0) {
            letters = letters.padEnd(size * (1 + letters.length / size), ' ')
        }

        return with(letters.chunked(size)) {
                this[0].indices.map { i ->
                    this.map { it[i] }.joinToString("")
                }
            }
            .joinToString(" ")
    }
}

