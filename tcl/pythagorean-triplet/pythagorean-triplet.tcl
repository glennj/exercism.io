# Algorithm stolen from Python example.py

namespace eval pythagoranTriplets {
    namespace export tripletsWithSum

    proc tripletsWithSum {perimeter} {
        set triplets {}
        foreach triplet [tripletsInRange 1 [expr {$perimeter / 2}]] {
            lassign $triplet a b c
            if {$a + $b + $c == $perimeter} {
                lappend triplets $triplet
            }
        }
        return $triplets
    }

    proc tripletsInRange {start end} {
        set triplets {}
        for {set limit 4} {$limit <= $end} {incr limit 4} {
            foreach triplet [primitiveTriplets $limit] {
                lassign $triplet x y z
                lassign $triplet a b c
                while {$a < $start} {
                    incr a $x
                    incr b $y
                    incr c $z
                }
                while {$c <= $end} {
                    lappend triplets [list $a $b $c]
                    incr a $x
                    incr b $y
                    incr c $z
                }
            }
        }
        return $triplets
    }

    proc primitiveTriplets {limit} {
        set triplets {}
        foreach pair [euclidianCoprimes $limit] {
            lassign $pair m n
            set a [expr {$m**2 - $n**2}]
            set b [expr {2 * $m * $n}]
            set c [expr {$m**2 + $n**2}]
            if {$a > $b} {
                lassign [list $b $a] a b
            }
            lappend triplets [list $a $b $c]
        }
        return $triplets
    }

    proc euclidianCoprimes {limit} {
        set mn [expr {$limit / 2}]
        set sqrt [expr {isqrt($mn)}]
        set pairs {}
        for {set n 1} {$n <= $sqrt} {incr n} {
            if {$mn % $n == 0} {
                set m [expr {$mn / $n}]
                if {($m - $n) % 2  == 1 && gcd($m, $n) == 1} {
                    lappend pairs [list $m $n]
                }
            }
        }
        return $pairs
    }
}

namespace import pythagoranTriplets::*


proc ::tcl::mathfunc::gcd {a b} {
    if {$b == 0} {
        return  $a
    }
    set procname [lindex [info level 0] 0]
    tailcall $procname $b [expr {$a % $b}]
}


# For posterity, a brute force solution.
# Noting that the smallest triple is {3 4 5}, we'll use those
# values in the loop boundaries to save a few iterations.

proc tripletsWithSum-brute {perimeter} {
    set triplets {}
    for {set c [expr {$perimeter - 3 - 4}]} {$c >= 5} {incr c -1} {
        for {set b [expr {$c - 1}]} {$b >= 4} {incr b -1} {
            set a [expr {$perimeter - $c - $b}]
            if {$a >= 3 && $a < $b && $a**2 + $b**2 == $c**2} {
                lappend triplets [list $a $b $c]
            }
        }
    }
    return $triplets
}

# benchmarking:
#   % source pythagorean-triplet.tcl
#   % time {tripletsWithSum 30000} 2
#   97062.424 microseconds per iteration
#   % time {tripletsWithSum-brute 30000} 2
#   63254745.4885 microseconds per iteration
