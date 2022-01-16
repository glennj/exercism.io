proc steps {n {step 0}} {
    if {![string is integer -strict $n] || $n < 1} {
        error "Only positive integers are allowed"
    }
    if {$n == 1} then {return $step}
    tailcall steps [expr {$n % 2 == 0 ? $n/2 : 3*$n + 1}] [incr step]
}
