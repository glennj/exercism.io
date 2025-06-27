proc bottleSong {start take} {
    set lines {}
    
    for {set i $start} {$i >= $start - $take + 1} {incr i -1} {
        set line1 "[bottle $i] hanging on the wall,"
        set line2 "There'll be [string tolower [bottle [expr {$i - 1}]]] hanging on the wall."
        lappend lines "" $line1 $line1 {And if one green bottle should accidentally fall,} $line2
    }

    lrange $lines 1 end
}

proc bottle n {
    set nums {No One Two Three Four Five Six Seven Eight Nine Ten}
    string cat [lindex $nums $n] " green bottle" [expr {$n == 1 ? "" : "s"}]
}
