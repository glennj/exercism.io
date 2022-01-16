# This is a very poorly specified exercise. A lot of guesswork in here.

oo::class create Scale {
    variable tonic 
    variable notes

    constructor {aTonic} {
        set tonic $aTonic
    }

    method chromatic {} {
        if {[info exists notes]} then {return $notes}

        set chromatic {
            sharps {A A# B C C# D D# E F F# G G#}
            flats  {A Bb B C Db D Eb E F Gb G Ab}
        }
        set tonics {
            sharps {G D A E B  "F# major" e b  f# c# g# "d# minor"}
            flats  {d g c f bb "eb minor" F Bb Eb Ab Db "Gb major"}
        }
        set scale [expr {
            $tonic in [dict get $tonics flats] ? "flats" : "sharps"
        }]
        set notes [dict get $chromatic $scale]

        # rotate the notes so the tonic comes first
        set idx [lsearch -exact $notes [string totitle $tonic]]
        set notes [list \
            {*}[lrange $notes $idx end] \
            {*}[lrange $notes 0 $idx-1] ]

        # what about the "XX major/minor" tonics?
        # Should the above be:
        #    lsearch -exact $notes [string totitle [lindex $tonic 0]]
        # ?
    }

    method intervals {intervals} {
        my chromatic
        set n [llength $notes]
        set idx 0
        set scale [lindex $notes $idx]

        foreach interval [split $intervals ""] {
            incr idx [my Interval $interval]
            lappend scale [lindex $notes [expr {$idx % $n}]]
        }
        return $scale
    }

    method Interval {interval} {
        try {
            return [dict get {m 1 M 2 A 3} $interval]
        } on error {} {
            error "Invalid interval: $interval"
        }
    }
}
