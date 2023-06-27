source seq.tcl

namespace eval TwelveDays {
    namespace import ::seq::seq
    namespace export verse sing

    variable ordinal {
        "--zero--"
        first second third fourth fifth sixth
        seventh eighth ninth tenth eleventh twelfth
    }

    variable gifts {
        "--zero--"
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

    variable fmt {On the %s day of Christmas my true love gave to me: %s.}

    proc verse {n} {
        variable ordinal
        variable gifts
        variable fmt

        set presents [lreverse [lrange $gifts 1 $n]]
        if {$n > 1} {
            lset presents end "and [lindex $presents end]"
        }
        format $fmt [lindex $ordinal $n] [join $presents ", "]
    }

    proc sing {from to} {
        set days [lmap i [seq -from $from -to $to] {verse $i}]
        return [join $days \n]
    }
}

namespace import TwelveDays::*
