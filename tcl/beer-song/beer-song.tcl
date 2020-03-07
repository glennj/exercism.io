namespace eval BeerSong {
    namespace export beerSong

    proc beerSong {start take {w "on the wall"}} {
        return [join [lmap n [verseNumbers $start $take] {
            string cat [first $n $w] "\n" [second $n $w]
        }] "\n"]
    }

    proc verseNumbers {start take} {
        for {set i 0} {$i < $take} {incr i} {
            lappend verseNumbers [expr {$start - $i}]
        }
        return $verseNumbers
    }

    proc first {n w} {
        set b [bottles $n]
        return "[string totitle $b] $w, $b."
    }

    proc second {n w} {
        return "[action $n], [bottles [incr n -1]] $w."
    }

    proc bottles {n} {
        return "[expr {howMany($n)}] bottle[expr {s($n)}] of beer"
    }

    proc action {n} {
        if {$n == 0} {
            return "Go to the store and buy some more"
        } else {
            return "Take [expr {one($n)}] down and pass it around"
        }
    }
}

namespace eval ::tcl::mathfunc {
    proc howMany {n} {expr {$n == 0 ? "no more" : ($n == -1 ? 99 : $n)}}
    proc s       {n} {expr {$n == 1 ? "" : "s"}}
    proc one     {n} {expr {$n == 1 ? "it" : "one"}}
}

namespace import BeerSong::beerSong
