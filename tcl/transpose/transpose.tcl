proc transpose {rows} {
    if {[llength $rows] == 0} {
        return {}
    }

    set maxWid [::tcl::mathfunc::max {*}[lmap row $rows {string length $row}]]

    set transposed {}
    for {set i 0} {$i < $maxWid} {incr i} {
        set col [lmap row $rows {
            set char [string index $row $i]
            expr {$char eq "" ? " " : $char}
        }]
        lappend transposed [string trimright [join $col ""]]
    }

    # each preceding line is at least as long as its successor
    for {set i [expr {[llength $transposed] - 2}]} {$i >= 0} {incr i -1} {
        set wid [string length [lindex $transposed $i+1]]
        lset transposed $i [format "%-*s" $wid [lindex $transposed $i]]
    }
    return $transposed
}
