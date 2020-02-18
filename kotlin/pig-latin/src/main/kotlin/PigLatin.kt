object PigLatin {

    fun translate(phrase: String): String =
            phrase.split(' ')
                  .joinToString(" ", transform = ::translateWord)

    private fun translateWord(word: String): String {
        var m = Regex("([aeiou]|yt|xr).*").matchEntire(word)
        if (m != null)
            return word + "ay"

        m = Regex("(.?qu)(.*)").matchEntire(word)
            ?: Regex("([^aeiou]+)(y.*)").matchEntire(word)
            ?: Regex("([^aeiou]+)(.*)").matchEntire(word)
        if (m != null)
            return m.groupValues[2] + m.groupValues[1] + "ay"

        return word
    }
}
