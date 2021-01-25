lappend auto_path ../lib
package require dictGetdef


proc roster {students} {
    # first, map the name to the grade: if a student has multiple
    # grades, they will be excluded later
    set names {}
    foreach student $students {
        lassign $student name grade
        dict lappend names $name $grade
    }

    # then, map the grade to its students
    set roster [dict create]
    dict for {name grades} $names {
        # ignore student in multiple grades
        if {[llength $grades] > 1} then continue
        dict lappend roster $grades $name
    }

    # sort the names within the grades
    dict for {grade names} $roster {
        dict set roster $grade [lsort $names]
    }

    # then sort the grades
    lsort -integer -stride 2 $roster
}


proc grade {students grade} {
    dict getdef [roster $students] $grade {}
}
