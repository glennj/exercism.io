class Anagram(val source: String) {
    private val src = source.toLowerCase()
    private val key = anagramKey(src)

    private fun anagramKey(word: String): List<Char> =
            word.toList().sorted()

    fun match(anagrams: Collection<String>): Set<String> =
            anagrams.map(String::toLowerCase)
                    .filterNot { src == it }
                    .filter { key == anagramKey(it) }
                    .toSet()
}
