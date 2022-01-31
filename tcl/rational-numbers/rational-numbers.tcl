source ./mathfunc.tcl

oo::class create Rational {
    variable num den
    constructor {numerator denominator} {
        set num $numerator
        set den $denominator

        if {$den < 0} {
            set num [expr {-1 * $num}]
            set den [expr {-1 * $den}]
        }
        set gcd [expr {gcd($num, $den)}]
        set num [expr {$num / $gcd}]
        set den [expr {$den / $gcd}]
    }

    method numerator   {} {return $num} 
    method denominator {} {return $den}

    method toString {} {
        string cat "$num/$den"
    }

    method recip {} {
        [self class] new $den $num
    }

    method add {other} {
        [self class] new \
            [expr {$num * [$other denominator] + $den * [$other numerator]}] \
            [expr {$den * [$other denominator]}]
    }

    method sub {other} {
        [self class] new \
            [expr {$num * [$other denominator] - $den * [$other numerator]}] \
            [expr {$den * [$other denominator]}]
    }

    method mul {other} {
        [self class] new \
            [expr {$num * [$other numerator]}] \
            [expr {$den * [$other denominator]}]
    }

    method div {other} {
        my mul [$other recip]
    }

    method abs {} {
        [self class] new [expr {abs($num)}] [expr {abs($den)}]
    }

    method pow {n} {
        set result [[self class] new [expr {$num ** abs($n)}] [expr {$den ** abs($n)}]]
        expr {$n >= 0 ? $result : [$result recip]}
    }

    method exprational {real} {
        expr {nthroot(double($real) ** $num, $den)}
    }
}
