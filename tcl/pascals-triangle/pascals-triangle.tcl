proc triangle {n} {
    set triangle {}
    for {set i 1} {$i <= $n} {incr i} {
        set prev [lindex $triangle end]
        set row {1}
        for {set j 1} {$j < $i} {incr j} {
            set a [lindexOrDefault 0 $prev [expr {$j - 1}]]
            set b [lindexOrDefault 0 $prev $j]
            lappend row [expr {$a + $b}]
        }
        lappend triangle $row
    }
    return $triangle
}


proc lindexOrDefault {defValue list args} {
    set value [lindex $list {*}$args]
    return [expr {$value == "" ? $defValue : $value}]
}
