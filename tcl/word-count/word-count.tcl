proc countWords {sentence} {
    set counts [dict create]

    # Normalize word separators, and remove leading/trailing whitespace
    set sentence [string trim [regsub -all {[\s,]+} $sentence " "]]

    foreach word [split $sentence] {
        # remove non-word non-hyphen characters
        set word [regsub -all {[^\w']} [string tolower $word] ""]

        # and remove outer hyphens while incrementing
        dict incr counts [string trim $word {'}]
    }
    return $counts
}
