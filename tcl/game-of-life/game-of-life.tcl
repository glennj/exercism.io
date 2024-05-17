namespace eval Conway {
    namespace export tick
    
    proc tick matrix {
        set rows [llength $matrix]
        set cols [llength [lindex $matrix 0]]
        set next $matrix
    
        for {set r 0} {$r < $rows} {incr r} {
            for {set c 0} {$c < $cols} {incr c} {
                switch [countNeighbours $r $c $matrix] {
                    3 {lset next $r $c 1}
                    2 {}
                    default {lset next $r $c 0}
                }
            }
        }
        return $next
    }
    
    variable deltas {
        {-1 -1} {-1 0} {-1 1}
        { 0 -1}        { 0 1}
        { 1 -1} { 1 0} { 1 1}
    }

    proc countNeighbours {r c matrix} {
        variable deltas
        foreach delta $deltas {
            lassign $delta dr dc
            set cell [lindex $matrix $r+$dr $c+$dc]
            if {$cell == ""} continue
            incr count $cell
        }
        return $count
    }
}

namespace import Conway::tick
