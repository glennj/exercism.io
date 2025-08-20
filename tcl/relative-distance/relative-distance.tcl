proc degreeOfSeparation {personA personB familyTree} {
    set neighbours [getNeighboursFromTree $familyTree]
    set queue      [list [list $personA 0]]
    set visited    [dict create $personA true]

    while {[llength $queue] > 0} {
        set pair [lpop queue 0]
        lassign $pair person degree

        if {$person eq $personB} {
            return $degree
        }

        incr degree
        foreach neighbour [dict get $neighbours $person] {
            if {[dict getdef $visited $neighbour false]} then continue
            dict set visited $neighbour true
            lappend queue [list $neighbour $degree]
        }
    }
    return -1 ;# not related
}

############################################################
proc getNeighboursFromTree {familyTree} {
    set neighbours [dict create]

    dict for {parent children} $familyTree {
        foreach child $children {
            addIfAbsent neighbours $parent $child
            addIfAbsent neighbours $child $parent

            foreach sibling $children {
                if {$child eq $sibling} then continue
                addIfAbsent neighbours $child $sibling
                addIfAbsent neighbours $sibling $child
            }
        }
    }
    return $neighbours
}

proc addIfAbsent {dictVarName key value} {
    upvar 1 $dictVarName dictVar
    if {[lsearch -exact [dict getdef $dictVar $key {}] $value] == -1} {
        dict lappend dictVar $key $value
    }
}
