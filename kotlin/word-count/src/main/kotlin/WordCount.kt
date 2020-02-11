object WordCount {

    fun phrase(phrase: String): Map<String, Int> {

        // A word is letters and numbers,
        // optionally followed by an apostrophe and more letters and numbers

        return Regex("\\p{Alnum}+(?:[']\\p{Alnum}+)?").findAll(phrase)

                .groupingBy { matchResult -> matchResult.value.toLowerCase() }
                .eachCount()

                //.groupBy { matchResult -> matchResult.value.toLowerCase() }
                //.mapValues { entry -> entry.value.size }
    }
}
