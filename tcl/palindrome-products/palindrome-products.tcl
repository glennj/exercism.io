# using algorithm from ILoveMuffins's python solution:
# https://exercism.io/tracks/python/exercises/palindrome-products/solutions/d68cab86cad94d4d821f26da44bb0722
#
namespace eval PalindromeProducts {
    proc main {smallestOrLargest min max} {
        assert {$min <= $max} "min must be <= max"
        assert {$smallestOrLargest in {smallest largest}} \
            "first arg must be 'smallest' or 'largest'" 

        # set up loop boundary variables
        $smallestOrLargest {productStart productEnd step factorStart} $min $max

        # rare case of not using braces to define a proc
        proc ::tcl::mathfunc::inRange {n} "expr {$min <= \$n && \$n <= $max}"

        set result {}
        for {set product $productStart} {cmp($product, $productEnd)} {incr product $step} {
            if {$product eq [string reverse $product]} {
                set factors [factors $product $step $factorStart]
                if {[llength $factors] > 0} {
                    # the first product that is found will be 
                    # the smallest or largest, so return immediately
                    return [list $product $factors]
                }
            }
        }

        # none found
        return {-1 {}}
    }

    proc smallest {vars min max} {
        foreach var $vars {upvar 1 $var $var}
        set productStart [expr {$min ** 2}]
        set productEnd [expr {$max ** 2}]
        set step 1
        set factorStart $min
        # the loop condition function
        proc ::tcl::mathfunc::cmp {a b} {expr {$a <= $b}}
    }

    proc largest {vars min max} {
        foreach var $vars {upvar 1 $var $var}
        set productStart [expr {$max ** 2}]
        set productEnd [expr {$min ** 2}]
        set step -1
        set factorStart $max
        # the loop condition function
        proc ::tcl::mathfunc::cmp {a b} {expr {$a >= $b}}
    }

    proc factors {n step i} {
        set factors {}
        for {} {cmp($i * $i, $n)} {incr i $step} {
            set j [expr {$n / $i}]
            if {$i * $j == $n && inRange($j)} {
                lappend factors [lsort -integer [list $i $j]]
            }
        }
        return $factors
    }
}

interp alias {} palindromeProducts {} PalindromeProducts::main


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
