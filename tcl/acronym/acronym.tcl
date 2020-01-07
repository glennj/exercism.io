proc abbreviate {phrase} {
    # Replace all non-alphabetic non-apostrophe characters with space.
    set phrase [regsub -all {[^[:alpha:]']} $phrase " "]

    # Find the first character of each word.  Note that `split` splits on
    # individual spaces, not sequences of whitespace: some of the words will
    # be zero-length.  But the first character of a zero-length string is
    # the empty string.
    set acronym ""
    foreach word [split $phrase] {
        append acronym [string range $word 0 0]
    }

    return [string toupper $acronym]
}
