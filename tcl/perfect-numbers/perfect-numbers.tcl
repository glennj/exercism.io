source ../lib/set.tcl

namespace eval PerfectNumbers {

    # The aliquot sum is defined as the sum of the factors of a number
    # not including the number itself.
    #
    proc aliquotSum {n} {
        set factors [Set new]
        for {set i [expr {isqrt($n)}]} {$i > 0} {incr i -1} {
            if {$n % $i == 0} {
                $factors add $i [expr {$n / $i}]
            }
        }
        $factors remove $n
        ::tcl::mathop::+ {*}[$factors toList]
    }

    proc classify {n} {
        set sum [aliquotSum $n]
        if {$sum == $n} { return perfect }
        if {$sum >  $n} { return abundant }
        if {$sum <  $n} { return deficient }
    }

    namespace export classify
}

namespace import PerfectNumbers::classify
