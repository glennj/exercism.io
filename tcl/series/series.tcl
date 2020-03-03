proc slices {str len} {
    assert {$str ne ""} "series cannot be empty"
    assert {$len >= 1} "slice length cannot be less than one"

    set strlen [string length $str]
    assert {$strlen >= $len} "slice length cannot be greater than series length"

    set slices [list]
    for {set i 0; set j $len} {$i <= $strlen - $len} {incr i; incr j} {
        lappend slices [string range $str $i $j-1]
    }
    return $slices
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
