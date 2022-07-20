#! tclsh

oo::class create HighScores {
    variable scores

    constructor {} {
        set scores [list]
    }
    method addScores {args} {
        lappend scores {*}$args
    }

    method scores {} {
        return $scores
    }

    method latest {} {
        return [lindex $scores end]
    }
    
    method personalBest {} {
        set max ""
        if {[llength $scores] > 0} {
            set max [::tcl::mathfunc::max {*}$scores]
        }
        return $max
    }

    method topThree {} {
        return [lrange [lsort -integer -decreasing $scores] 0 2]
    }
}
