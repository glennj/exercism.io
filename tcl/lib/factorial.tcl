namespace eval factorial {
    namespace import ::tcl::mathop::-
    namespace import ::tcl::mathop::*

    proc factorial {n {f 1}} {
        if {$n == 0} {
            return $f
        }
        tailcall factorial [- $n 1] [* $n $f]
    }
}

proc fact {n} {
    if {![string is integer $n] || $n < 0} {
        error "n must be a positive integer"
    }
    ::factorial::factorial $n
}
