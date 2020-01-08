namespace eval triangle {
    proc is {type sides} {
        lassign [lsort -real $sides] a b c

        # valid triangle?
        if {!($a > 0 && $a + $b > $c)} {
            return false
        }

        set nUniq [llength [lsort -real -unique $sides]]
        
        switch -exact -- $type {
            equilateral {return [expr {$nUniq == 1}]}
            isosceles   {return [expr {$nUniq <= 2}]}
            scalene     {return [expr {$nUniq == 3}]}
            default     {error "invalid type: $type"}
        }
    }
}
