class Acronym {

    // a letter preceded by:
    // - start of string, or
    // - a non-letter, non-hyphen character
    static final regex = "(?:^|[^\\p{Alpha}'])(\\p{Alpha})"

    static String abbreviate(String phrase) {
        def matcher = phrase =~ regex
        matcher.inject("") { acc, match -> acc + match[1] }
               .toUpperCase()
    }

}
