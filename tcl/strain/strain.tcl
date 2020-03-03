  
proc keep {varname list condition {keep yes}} {
    upvar 1 $varname elem
    lmap elem $list {
        if {!![uplevel 1 $condition] == !!$keep} {
            set elem
        } else {
            continue
        }
    }
}

proc discard {varname list condition} {
    tailcall keep $varname $list $condition no
}
