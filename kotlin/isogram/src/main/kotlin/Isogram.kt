object Isogram {

    fun isIsogram(input: String): Boolean =
        input.toLowerCase()
             .filter(Char::isLetter)
             .groupingBy { it }
             .eachCount()
             .none { it.value > 1 }
}
