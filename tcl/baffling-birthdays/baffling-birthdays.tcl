# Determine if a list of YYYY-MM-DD birthdates contains
# any shared birthdays.

proc sharedBirthday {birthdays} {
    array set counts {}
    foreach date $birthdays {
        set md [string range $date 5 end]
        if {[info exists counts($md)]} {
            return true
        }
        set counts($md) true
    }
    return false
}

# Generate a list of $count random dates in YYYY-MM-DD form.
# Do not include leap years.

proc randomBirthdates {count} {
    set thisYear [clock format [clock seconds] -format %Y]

    set randomDate {{} {
        upvar thisYear thisYear
        set isLeap {{year} {
            expr {$year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0)}
        }}

        set year 0
        while {[apply $isLeap $year]} {
            set year [expr {int(rand() * ($thisYear - 1900)) + 1900}]
        }
        set dayOfYear [expr {int(rand() * 365) + 1}]
        set time [clock scan "$year:$dayOfYear" -format {%Y:%j}]
        clock format $time -format {%Y-%m-%d}
    }}

    set dates {}
    for {set i 0} {$i < $count} {incr i} {
        lappend dates [apply $randomDate]
    }
    return $dates
}

# Estimate the probability that 2 people in a group of given size
# have the same birthday.
#
# Using https://en.wikipedia.org/wiki/Birthday_problem#Calculating_the_probability

proc estimatedProbabilityOfSharedBirthday {size} {
    set Vnr [expr {[factorial 365] / [factorial [expr {365 - $size}]]}]
    set Vt [expr {365 ** $size}]
    set Pb [expr {1 - double($Vnr)/$Vt}]
    return [expr {100.0 * $Pb}]
}

proc factorial {n {f 1}} {
    if {$n == 0} then {return $f}
    tailcall factorial [expr {$n - 1}] [expr {$n * $f}]
}
