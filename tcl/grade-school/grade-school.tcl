# This approach pre-sorts the data, then creates the mapping.
#
proc roster {students} {
    set flattened [concat {*}$students]

    # sort first by name (index 0 of each stride) 
    # then by grade (index 1)
    set sorted \
        [lsort -int  -index 1 -stride 2 \
            [lsort -dict -index 0 -stride 2 $flattened]]

    set roster [dict create]
    foreach {name grade} $sorted {
        dict lappend roster $grade $name
    }
    return $roster
}


proc grade {students grade} {
    set roster [roster $students]
    if {[catch {dict get $roster $grade} names] != 0} {
        set names {}
    }
    return $names
}
