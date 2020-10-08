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

    variable verse {On the %s day of Christmas my true love gave to me: %s.}

    proc verse {n} {
        variable ordinal
        variable gifts
        variable verse

        set days [lreverse [lrange $gifts 1 $n]]
        if {$n > 1} {
            lset days end "and [lindex $days end]"
        }
        format $verse [lindex $ordinal $n] [join $days ", "]
    }

    proc sing {from to} {
        for {set i $from} {$i <= $to} {incr i} {
            lappend days [verse $i]
        }
        return [join $days \n]
    }
}

namespace import TwelveDays::*
