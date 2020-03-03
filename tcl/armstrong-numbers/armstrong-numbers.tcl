# "everything is a string"
#
proc isArmstrongNumber_string {number} {
    # no special handling for zero required

    set len [string length $number]
    foreach digit [split $number ""] {
        incr sum [expr {$digit ** $len}]
    }
    expr {$number == $sum}
}

# mathematically
#
proc isArmstrongNumber_math {number} {
    if {$number == 0} then {return true}

    set len [expr {int(log10($number)) + 1}]
    set n $number
    while {$n > 0} {
        set digit [expr {$n % 10}]
        incr sum [expr {$digit ** $len}]
        set n [expr {$n / 10}]
    }
    expr {$number == $sum}
}

interp alias {} isArmstrongNumber {} isArmstrongNumber_math
