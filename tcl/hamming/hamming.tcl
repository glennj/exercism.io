proc hammingDistance {left right} {
    if {[string length $left] != [string length $right]} {
        error "strands must be of equal length"
    }

    set dist 0
    foreach L [split $left ""] R [split $right ""] {
        if {$L ne $R} {
            incr dist
        }
    }
    return $dist
}
