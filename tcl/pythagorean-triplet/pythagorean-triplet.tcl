proc tripletsWithSum {perimeter} {
    set triplets {}
    set a 0
    while {true} {
        incr a
        set num [expr {$perimeter * ($perimeter - 2 * $a)}]
        set den [expr {2 * ($perimeter - $a)}]
        set b   [expr {$num / $den}]
        if {$b < $a} then break
        if {$num % $den != 0} then continue
        set c [expr {$perimeter - $a - $b}]
        lappend triplets [list $a $b $c]
    }
    return $triplets
}
