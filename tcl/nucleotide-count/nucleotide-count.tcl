proc nucleotideCounts {strand} {
    set counts [dict create A 0 C 0 G 0 T 0]
    foreach char [split $strand ""] {
        if {[dict exists $counts $char]} {
            dict incr counts $char
        } else {
            error "Invalid nucleotide in strand"
        }
    }
    return $counts
}
