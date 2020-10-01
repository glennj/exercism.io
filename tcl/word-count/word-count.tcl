proc countWords {sentence} {
    set counts [dict create]

    # find all "words" -- numbers, letters and apostrophes,
    # trim leading/trailing apostrophes from each word,
    # and count them case insensitively

    foreach word [regexp -all -inline {[[:alnum:]']+} $sentence] {
        set w [string trim $word {'}]
        dict incr counts [string tolower $w]
    }
    return $counts
}
