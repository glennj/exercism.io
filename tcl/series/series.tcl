proc slices {str len} {
    if {$str eq ""} {
        error "series cannot be empty"
    }
    if {$len < 1} {
        error "slice length cannot be less than one"
    }

    set strlen [string length $str]
    if {$strlen < $len} {
        error "slice length cannot be greater than series length"
    }

    set slices [list]
    for {set i 0; set j $len} {$i <= $strlen - $len} {incr i; incr j} {
        lappend slices [string range $str $i $j-1]
    }
    return $slices
}
