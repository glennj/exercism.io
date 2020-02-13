object Pangram {

    fun isPangram(input: String): Boolean {

        // ordered by least-frequent usage:
        // https://en.wikipedia.org/wiki/Letter_frequency

        for (c in "ZQXJKVBPYGFWMUCLDRHSNIOATE")
            if (!input.contains(c, ignoreCase = true))
                return false
        return true
    }
}
