class Anagram(val source: String) {
    private val src = source.toLowerCase()
    private val key = anagramKey(src)

    private fun anagramKey(word: String): List<Char> =
            word.toList().sorted()

    fun match(anagrams: Collection<String>): Set<String> =
            anagrams.map { it to it.toLowerCase() }
                    .filterNot { src == it.second }
                    .filter { key == anagramKey(it.second) }
                    .map { it.first }
                    .toSet()
}
