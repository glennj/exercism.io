# This is a very poorly specified exercise. A lot of guesswork in here.

oo::class create Scale {
    variable tonic_ 
    variable notes_

    constructor {tonic} {
        set tonic_ $tonic
    }

    method chromatic {} {
        if {[info exists notes_]} then {return $notes_}

        set chromatic {
            sharps {A A# B C C# D D# E F F# G G#}
            flats  {A Bb B C Db D Eb E F Gb G Ab}
        }
        set tonics {
            sharps {G D A E B  "F# major" e b  f# c# g# "d# minor"}
            flats  {d g c f bb "eb minor" F Bb Eb Ab Db "Gb major"}
        }
        set scale [expr {
            $tonic_ in [dict get $tonics flats] ? "flats" : "sharps"
        }]
        set notes_ [dict get $chromatic $scale]

        # rotate the notes so the tonic comes first
        set idx [lsearch -exact $notes_ [string totitle $tonic_]]
        set notes_ [concat [lrange $notes_ $idx end] [lrange $notes_ 0 $idx-1]]

        # what about the "XX major/minor" tonics?
        # Should the above be:
        #    lsearch -exact $notes_ [string totitle [lindex $tonic_ 0]]
        # ?
    }

    method intervals {intervals} {
        set idx 0
        lmap interval [split $intervals ""] {
            set note [lindex [my chromatic] $idx]
            try {
                incr idx [dict get {m 1 M 2 A 3} $interval]
            } trap {TCL LOOKUP DICT} {} {
                error "invalid interval: $interval"
            }
            set note
        }
    }
}
