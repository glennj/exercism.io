proc secretHandshake {code} {
    set actions {wink "double blink" "close your eyes" jump}

    set handshake [lmapWithIndex {i action} $actions {
        if {($code & (1 << $i)) == 0} then continue
        set action
    }]

    if {($code & (1 << [llength $actions])) != 0} {
        return [lreverse $handshake]
    } else {
        return $handshake
    }
}


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
