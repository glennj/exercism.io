# Change making algorithm from
# http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
#
# Tcl doesn't have a NULL value. I'm using the string "null" instead.

namespace eval Change {
    namespace export findMinimumCoins

    proc findMinimumCoins {amount coins} {
        assert {$amount >= 0} "target can't be negative"
        
        # These algorithms assume 1-based array indexing: pad the coins list
        set denominations [concat null $coins]

        lassign [Change $denominations [llength $coins] $amount] C S
        return [MakeChange $S $denominations $amount]
    }

    # Compute the Value of the Optimal Solution Bottom-up. 
    # * d is the array of denomination values, 
    # * k is the number of denominations, and 
    # * n is the amount for which change is to be made
    #
    # Then
    # * C[p] will contain the correct minimum number of coins needed to make
    #   change for p cents, and
    # * S[p] will contain (the index of) the first coin in an optimal solution
    #   to making change for p cents.
    #
    proc Change {d k n} {
        set C [concat 0 [lrepeat $k null]]
        set S [concat null [lrepeat $k null]]

        for {set p 1} {$p <= $n} {incr p} {
            set min Inf
            set coin null
            for {set i 1} {$i <= $k} {incr i} {
                if {[lindex $d $i] <= $p} {
                    set idx [expr {$p - [lindex $d $i]}]
                    set possibleMin [expr {1 + [lindex $C $idx]}]
                    if {$possibleMin < $min} {
                        set min $possibleMin
                        set coin $i
                    }
                }
            }
            lset C $p $min
            lset S $p $coin
        }
        return [list $C $S]
    }

    proc MakeChange {S d n} {
        set change {}
        while {$n > 0} {
            set idx [lindex $S $n]
            assert {$idx ne "null"} "can't make target with given coins"
            set coin [lindex $d $idx]
            lappend change $coin
            incr n "-$coin"
        }
        return $change
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

namespace import Change::findMinimumCoins
