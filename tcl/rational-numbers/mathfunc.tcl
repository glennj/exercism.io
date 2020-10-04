package provide additionalMathFunctions 0.1

namespace eval ::tcl::mathfunc {
    proc gcd {a b} {
        if {$b == 0} {
            return $a
        }
        tailcall gcd $b [expr {$a % $b}]
    }

    proc nthroot {num root} {
        return [expr { exp(1) ** (log($num) / $root) }]
    }
}
