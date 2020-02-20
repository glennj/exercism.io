object AffineCipher {
    private val Alphabet = ('a'..'z').joinToString("")

    fun encode(input: String, a: Int, b: Int): String {
        require(a.isCoprimeWith(Alphabet.length)) { "a and m must be coprime." }
        return code(input) { x -> a * x + b }
                .chunked(5)
                .joinToString(" ")
    }

    fun decode(input: String, a: Int, b: Int): String {
        require(a.isCoprimeWith(Alphabet.length)) { "a and m must be coprime." }
        val aInv = a.mmi(Alphabet.length)
        return code(input) { y -> aInv * (y - b) }
    }

    private fun code(text: String, transform: (Int) -> Int): String =
        text.filter(Char::isLetterOrDigit)
            .map(Char::toLowerCase)
            .map { c ->
                when (val i = Alphabet.indexOf(c)) {
                    -1 -> c
                    else -> Alphabet[transform(i).floorMod(Alphabet.length)]
                }
            }
            .joinToString("")
}

fun Int.isCoprimeWith(other: Int): Boolean = this.gcd(other) == 1

// could use java.lang.BigInteger::gcd
tailrec fun Int.gcd(b: Int): Int = if (b == 0) this else b.gcd(this % b)

// return the modulus in the range (0 until this)
// could use java.lang.Math.floorMod
fun Int.floorMod(m: Int): Int = (this % m + m) % m

// could use java.lang.BigInteger::modInverse
fun Int.mmi(m: Int): Int = (0 until m).first { (this * it).floorMod(m) == 1 }
