object Acronym {
    // Match some letters optionally followed by an apostrophe
    // and more letters.
    // Rely on regex greediness to match whole words.
    private val wordPattern = "\\p{Alpha}+(?:'\\p{Alpha}+)?"

    fun generate(phrase: String) : String =
        Regex(wordPattern)
            .findAll(phrase)
            .map(MatchResult::value)
            .map(String::first)
            .map(Character::toUpperCase)
            .joinToString("")

}
