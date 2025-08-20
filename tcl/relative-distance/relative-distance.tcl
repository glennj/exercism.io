source ./dict.tcl

proc degreeOfSeparation {personA personB familyTree} {
    set neighbours [getNeighboursFromTree $familyTree]
    puts [list $neighbours]

    return 1
}

proc getNeighboursFromTree {familyTree} {
    set neighbours [dict create]

    dict for {parent children} $familyTree {
        foreach child $children {
            dict setIfAbsent neighbours $parent $child
            dict setIfAbsent neighbours $child $parent

            foreach sibling $children {
                if {$child ne $sibling} {
                    dict setIfAbsent neighbours $child $sibling
                    dict setIfAbsent neighbours $sibling $child
                }
            }
        }
    }

    return $neighbours
}
