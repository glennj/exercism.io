# A recursive solution to finding the combinations

proc combinations {sum size exclude} {
    if {$size == 1} {
        if {1 <= $sum && $sum <= 9 && $sum ni $exclude} {
            return $sum
        }
    } else {
        set procname [lindex [info level 0] 0]
        set results [dict create]
        foreach n {1 2 3 4 5 6 7 8 9} {
            if {$n ni $exclude} {
                foreach comb [$procname \
                                [expr {$sum - $n}] \
                                [expr {$size - 1}] \
                                [concat $exclude $n]] {
                    dict set results [lsort [concat $comb $n]] 1
                }
            }
        }
        return [dict keys $results]
    }
}
