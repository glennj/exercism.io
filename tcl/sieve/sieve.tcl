proc primes {limit} {
    if {$limit < 2} then {return {}}

    set candidates [lrepeat [expr {$limit + 1}] true]

    lappend primes 2
    markMultiplesOf 2 candidates $limit

    for {set p 3} {$p <= $limit} {incr p 2} {
        if {[lindex $candidates $p]} {
            lappend primes $p
            markMultiplesOf $p candidates $limit
        }
    }

    return $primes
}


proc markMultiplesOf {p varName limit} {
    upvar 1 $varName candidates
    set step [expr {$p == 2 ? 2 : $p * 2}]
    for {set i [expr {$p * $p}]} {$i <= $limit} {incr i $step} {
        lset candidates $i false
    }
}
