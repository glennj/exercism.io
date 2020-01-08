namespace eval TwelveDays {
    namespace export verse sing

    variable ordinal {
        "" first second third fourth fifth sixth
        seventh eighth ninth tenth eleventh twelfth
    }

    variable gifts {
        ""
        "a Partridge in a Pear Tree"
        "two Turtle Doves"
        "three French Hens"
        "four Calling Birds"
        "five Gold Rings"
        "six Geese-a-Laying"
        "seven Swans-a-Swimming"
        "eight Maids-a-Milking"
        "nine Ladies Dancing"
        "ten Lords-a-Leaping"
        "eleven Pipers Piping"
        "twelve Drummers Drumming"
    }

    proc verse {n} {
        variable ordinal
        variable gifts

        set verse "On the [lindex $ordinal $n] day of Christmas "
        append verse "my true love gave to me: "

        set days [lreverse [lrange $gifts 1 $n]]
        if {$n > 1} {
            lset days end "and [lindex $days end]"
        }
        append verse [join $days ", "]
        return "$verse."
    }

    proc sing {from to} {
        for {set i $from} {$i <= $to} {incr i} {
            lappend days [verse $i]
        }
        return [join $days \n]
    }
}

namespace import TwelveDays::*
