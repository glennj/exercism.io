namespace import ::tcl::mathfunc::min
namespace import ::tcl::mathfunc::max

proc saddlePoints {matrix} {
    if {[llength $matrix] == 0 || [llength [lindex $matrix 0]] == 0} {
        return {}
    }

    set rowMax [lmap row $matrix {max {*}$row}]
    set colMin [lmap col [transpose $matrix] {min {*}$col}]
    set saddlePoints {}

    for {set r 0} {$r < [llength $matrix]} {incr r} {
        for {set c 0} {$c < [llength [lindex $matrix 0]]} {incr c} {
            set elem [lindex $matrix $r $c]
            if {$elem == [lindex $rowMax $r] && $elem == [lindex $colMin $c]} {
                lappend saddlePoints [list [expr {$r+1}] [expr {$c+1}]]
            }
        }
    }
    return $saddlePoints
}

proc transpose {matrix} {
    lmap i [openRange 0 [llength [lindex $matrix 0]]] {
        lmap row $matrix {lindex $row $i}
    }
}

proc openRange {start stop} {
    for {set x $start} {$x < $stop} {incr x} {
        lappend xs $x
    }
    return $xs
}
