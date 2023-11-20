namespace eval ::factorial {
    namespace import ::tcl::mathop::-
    namespace import ::tcl::mathop::*

    proc factorial {n {f 1}} {
        if {$n == 0} {
            return $f
        }
        tailcall factorial [- $n 1] [* $n $f]
    }
}

proc fact_recursive {n} {
    if {![string is integer $n] || $n < 0} {
        error "n must be a positive integer"
    }
    ::factorial::factorial $n
}


proc fact_iterative {n} {
    set f 1
    for {set i 2} {$i <= $n} {incr i} {
        set f [expr {$f * $i}]
    }
    return $f
}


# some timing:
#
#    % time {fact_recursive 1000} 10
#    1487.6 microseconds per iteration
#
#    % time {fact_iterative 1000} 10
#    852.7 microseconds per iteration
#
# but at some point, there's a crossover
# where the recursive proc becomes faster
#
#    % time {fact_recursive 10000} 10
#    58911.0 microseconds per iteration
#
#    % time {fact_iterative 10000} 10
#    99507.7 microseconds per iteration
