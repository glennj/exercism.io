proc slices {str len {method byRunningSlice}} {
    assert {$str ne ""} "series cannot be empty"
    assert {$len >= 1} "slice length cannot be less than one"

    set strlen [string length $str]
    assert {$strlen >= $len} "slice length cannot be greater than series length"

    $method $str $strlen $len
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

# using successive string ranges
proc byStringRange {str strlen len} {
    set slices [list]
    for {set i 0; set j $len} {$j <= $strlen} {incr i; incr j} {
        lappend slices [string range $str $i $j-1]
    }
    return $slices
}

# using a "running" slice
proc byRunningSlice {str strlen len} {
    # initial slice
    set slice [string range $str 0 $len-1]
    set slices [list $slice]

    for {set i $len} {$i < $strlen} {incr i} {
        set slice "[string range $slice 1 end][string index $str $i]"
        lappend slices $slice
    }
    return $slices
}


# benchmarking
if {$argv0 eq [info script]} {
    set str [string repeat asdflkjhqweroiuyxzvbzcxvb 100]
    foreach method {byStringRange byRunningSlice} {
        puts [list $method [time {slices $str 10 $method} 10000]]
    }
}
