proc annotate {field} {
    # translate the field to a matrix of integers
    set matrix [lmap row $field {
        lmap char [split $row ""] {
            expr {$char == "*" ? -1 : 0}
        }
    }]

    set height [llength $matrix]
    set width [llength [lindex $matrix 0]]

    # Look for flowers, and increment its neighbours
    for {set r 0} {$r < $height} {incr r} {
        for {set c 0} {$c < $width} {incr c} {
            if {[lindex $matrix $r $c] == -1} {
                # it's a flower
                foreach dr {-1 0 1} {
                    set rr [expr {$r + $dr}]
                    if {$rr < 0 || $rr == $height} continue ;# out of bounds
                    foreach dc {-1 0 1} {
                        set cc [expr {$c + $dc}]
                        if {$cc < 0 || $cc == $width} continue ;# out of bounds
                        set cell [lindex $matrix $rr $cc]
                        if {$cell != -1} {
                            # this neighbour is not a flower
                            lset matrix $rr $cc [incr cell]
                        }
                    }
                }
            }
        }
    }

    # form the annotated field
    lmap row $matrix {
        join [lmap cell $row {
            expr {$cell == -1 ? "*" : ($cell == 0 ? " " : $cell)}
        }] ""
    }
}
