# Inspired by this javascript solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

namespace eval SayNumber {
    namespace export say

    proc say {n} {
        if {$n < 0 || $n >= 1e12} then {error "input out of range"}

        if     {$n < 100}  { saySmall $n } \
        elseif {$n < 1e3}  { sayCompound $n hundred 100 } \
        elseif {$n < 1e6}  { sayCompound $n thousand 1000 } \
        elseif {$n < 1e9}  { sayCompound $n million 1000000 } \
        else               { sayCompound $n billion 1000000000 }
    }

    proc divmod {n divisor} {
        list [expr {$n / $divisor}] [expr {$n % $divisor}]
    }

    proc saySmall {n} {
        lassign [divmod $n 10] div rem

        if {$n < 20} {
            lindex {
                zero one two three four five six seven eight nine
                ten eleven twelve thirteen fourteen
                fifteen sixteen seventeen eightteen nineteen
            } $n

        } elseif {$rem == 0} {
            lindex {
                "" "" twenty thirty forty fifty sixty seventy eighty ninety
            } $div

        } else {
            format "%s-%s" [saySmall [expr {$div * 10}]] [saySmall $rem]
        }
    }

    proc sayCompound {n group divisor} {
        lassign [divmod $n $divisor] div rem
        # using `concat` for the "list flattening" effect
        set result [concat [say $div] $group]
        if {$rem != 0} then {lappend result {*}[say $rem]}
        return $result
    }
}

namespace import SayNumber::say
