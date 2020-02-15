object Atbash {

    fun encode(s: String): String = decode(s).groupsOf()

    fun decode(s: String): String =
            s.filter(Character::isLetterOrDigit)    // s.replace(Regex("[^\\p{Alnum}]"), "")
                    .toLowerCase()
                    .map { encoding[it] ?: it }
                    .joinToString("")

    // I now realize I'm reimplementing String::chunked
    private fun String.groupsOf(size: Int = 5): String =
        Regex(".{1,$size}")
                .findAll(this)
                .map(MatchResult::value)
                .joinToString(" ")

    private val encoding = with('a'..'z') { this.zip(this.reversed()).toMap() }
}
