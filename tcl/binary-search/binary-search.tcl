proc binarySearch {haystack needle {left 0} {right ""}} {
    if {$right eq ""} {
        set right [expr {[llength $haystack] - 1}]
    }

    if {$left > $right} {
        return -1
    }

    set mid [expr {($left + $right) / 2}]
    set elem [lindex $haystack $mid]
    
    if {$elem == $needle} {
        return $mid
    }

    if {$needle < $elem} {
        set right [expr {$mid - 1}]
    } else {
        set left [expr {$mid + 1}]
    }

    set procname [lindex [info level 0] 0]
    tailcall $procname $haystack $needle $left $right
}
