namespace eval primeNumbers {
    namespace export nthPrime

    variable primes {}

    proc nthPrime {n} {
        assert {$n >= 1} "there is no zeroth prime"
        variable primes
        while {$n > [llength $primes]} {nextPrime}
        return [lindex $primes $n-1]
    }

    proc generator {} {
        variable primes
        yield [info coroutine]
        lappend primes 2
        yield 2
        for {set i 3} {true} {incr i 2} {
            if {[isPrime $i]} {
                lappend primes $i
                yield $i
            }
        }
    }

    coroutine nextPrime [namespace current]::generator

    proc isPrime {n} {
        variable primes
        foreach p $primes {
            if {$p * $p > $n} break
            if {$n % $p == 0} {return false}
        }
        return true
    }
}

namespace import primeNumbers::nthPrime


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
