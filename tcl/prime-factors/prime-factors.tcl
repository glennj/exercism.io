proc factors {n {p 2} {primeFactors {}}} {
    if {$p * $p > $n} {
        if {$n > 1} then {lappend primeFactors $n }
        return $primeFactors
    }

    while {$n % $p == 0} {
        lappend primeFactors $p
        set n [expr {$n / $p}]
    }

    set procname [lindex [info level 0] 0]
    set step [expr {$p == 2 ? 1 : 2}]

    tailcall $procname $n [incr p $step] $primeFactors
}
