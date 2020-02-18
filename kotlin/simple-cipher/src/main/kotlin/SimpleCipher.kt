import java.lang.Math

data class Cipher(var key: String = randomKey()) {

    companion object {
        const val ALPHABET = "abcdefghijklmnopqrstuvwxyz"
        fun randomKey(): String =
                (1..100).map { ALPHABET.random() }.joinToString("")
    }

    init {
        require(key.isNotEmpty())
        require(key.all { it in ALPHABET })
    }

    fun encode(s: String): String = code(s, +1)
    fun decode(s: String): String = code(s, -1)

    private fun code(s: String, direction: Int): String {
        require(s.all { it in ALPHABET })
        // ensure key is at least as long as the string
        while (key.length < s.length) key += key

        return s.zip(key)
                .map { codeChar(it.first, it.second, direction) }
                .joinToString("")
    }

    private fun codeChar(textChar: Char, keyChar: Char, dir: Int): Char {
        val idx = Math.floorMod(
                ALPHABET.indexOf(textChar) + dir * ALPHABET.indexOf(keyChar),
                ALPHABET.length
        )
        return ALPHABET[idx]
    }
}

