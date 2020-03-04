
proc lmapWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set idx -1
    lmap elem $list {
        incr idx
        uplevel 1 $body
    }
}

proc foreachWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set len [llength $list]
    for {set idx 0} {$idx < $len} {incr idx} {
        set elem [lindex $list $idx]
        uplevel 1 $body
    }
}

proc withIndex {list} {
    set len [llength $list]
    for {set i 0} {$i < $len} {incr i} {
        lappend result $i [lindex $list $i]
    }
    return $result
}
proc select {elemVar list body} {
    upvar 1 $elemVar elem
    lmap elem $list {
        if {[uplevel 1 $body]} {
            set elem
        } else {
            continue
        }
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}


proc foldlCoro {initValue varnames coro body} {
    lassign $varnames accumVarname elemVarname
    upvar 1 $accumVarname acc
    upvar 1 $elemVarname  elem

    set acc $initValue
    while {[set elem [$coro]] ne ""} {
        set acc [uplevel 1 $body]
    }
    return $acc
}

proc foldl {initValue varnames list body} {
    lassign $varnames accumVarname elemVarname
    upvar 1 $accumVarname acc
    upvar 1 $elemVarname  elem

    set acc $initValue
    foreach elem $list {
        set acc [uplevel 1 $body]
    }
    return $acc
}
