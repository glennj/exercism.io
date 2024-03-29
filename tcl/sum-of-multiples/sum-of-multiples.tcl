source set.tcl

proc sumOfMultiples {factors limit} {
    set multiples [Set new]
    foreach f $factors {
        if {$f > 0} {
            for {set i 1} {$f * $i < $limit} {incr i} {
                $multiples add [expr {$f * $i}]
            }
        }
    }
    tcl::mathop::+ {*}[$multiples toList]
}
