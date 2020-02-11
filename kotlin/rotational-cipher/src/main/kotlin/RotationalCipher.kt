class RotationalCipher(val rotation: Int) {
    private var encoding: Map<String, String>

    init {
        require(rotation in 0..26) { "Invalid rotation." }

        var alphabet = ('A'..'Z').toList().map(Char::toString)
        var rotated = alphabet.drop(rotation) + alphabet.take(rotation)

        encoding = alphabet.zip(rotated).toMap().plus(
                alphabet.map(String::toLowerCase).zip(rotated.map(String::toLowerCase)).toMap()
        )
    }

    fun encode(text: String): String = text.replace(Regex("\\p{Alpha}")) {
        encoding[it.value] ?: it.value
    }
}
