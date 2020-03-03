# the mathematical approach
#
namespace eval difference::math {
    namespace export squareOfSum sumOfSquares differenceOfSquares

    proc squareOfSum {n} {
        expr { (($n * ($n + 1)) / 2) ** 2 }
    }

    proc sumOfSquares {n} {
        expr { $n * ($n + 1) * (2 * $n + 1) / 6 }
    }

    proc differenceOfSquares {n} {
        expr {abs( [sumOfSquares $n] - [squareOfSum $n] )}
    }
}

# an iterative approach
#
namespace eval difference::iterative {
    namespace export squareOfSum sumOfSquares differenceOfSquares

    # a simple generator
    proc sequence {n} {
        yield
        for {set i 1} {$i <= $n} {incr i} {
            yield $i
        }
    }

    proc foldl {initValue varnames coro body} {
        lassign $varnames accumVarname elemVarname
        upvar 1 $accumVarname acc
        upvar 1 $elemVarname  elem

        set acc $initValue
        while {[set elem [$coro]] ne ""} {
            set acc [uplevel 1 $body]
        }
        return $acc
    }

    proc squareOfSum {n} {
        coroutine seq sequence $n
        set sum [foldl 0 {sum x} seq {expr {$sum + $x}}]
        expr {$sum ** 2}
    }

    proc sumOfSquares {n} {
        coroutine seq sequence $n
        foldl 0 {sum x} seq {expr {$sum + $x**2}}
    }

    proc differenceOfSquares {n} {
        expr {abs( [sumOfSquares $n] - [squareOfSum $n] )}
    }
}

#namespace import difference::math::*
namespace import difference::iterative::*
