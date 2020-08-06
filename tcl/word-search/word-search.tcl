proc wordSearch {grid words} {
    return [[WordSearch new $grid $words] search]
}

############################################################
oo::class create WordSearch {
    variable grid height width
    variable words
    variable found

    constructor {wordGrid wordList} {
        set grid [lmap row $wordGrid {split $row ""}]
        set height [llength $grid]
        set width  [llength [lindex $grid 0]]

        # I want the words sorted by length (decreasing).
        # That way, I'll find "javascript" before "java".
        # A Schwartzian transform:
        set words [
            lmap pair [
                lsort -integer -decreasing -index 0 [
                    lmap elem $wordList {list [string length $elem] $elem}
                ]
            ] {lindex $pair 1}
        ]

        foreach word $words {
            dict set found $word {}
        }
    }

    method search {} {
        for {set r 0} {$r < $height} {incr r} {
            for {set c 0} {$c < $width} {incr c} {
                my _searchAt $r $c
            }
        }
        return $found
    }

    method _searchAt {r c} {
        set letter [lindex $grid $r $c]
        set possibles [lsearch -inline -glob $words "$letter*"]
        if {[llength $possibles] == 0} then return

        # clockwise from "north"
        set directions {
            -1  0
            -1  1
             0  1
             1  1
             1  0
             1 -1
             0 -1
            -1 -1
        }
        foreach {Δr Δc} $directions {
            set gridString [my _gridString $r $c ${Δr} ${Δc}]
            foreach word $possibles {
                if {[string match ${word}* $gridString]} {
                    my _found $word $r $c ${Δr} ${Δc}
                }
            }
        }
    }

    # Return the string of letters in the grid from
    # cell (r,c) out to the edge in the direction of 
    # the deltas.
    method _gridString {r c Δr Δc} {
        while {
            0 <= $r && $r < $height &&
            0 <= $c && $c < $width
        } {
            append string [lindex $grid $r $c]
            incr r ${Δr}
            incr c ${Δc}
        }
        return $string
    }

    method _found {word r c Δr Δc} {
        set len [string length $word]
        set endrow [expr {$r + ${Δr} * ($len - 1)}]
        set endcol [expr {$c + ${Δc} * ($len - 1)}]
        dict set found $word [list [my _coord $r $c] [my _coord $endrow $endcol]]
        lremove words $word
    }

    # the answer wants 1-based (x,y) coordinates
    method _coord {row col} {
        list [incr col] [incr row]
    }
}

############################################################
# helper proc: remove the first element matching the input
proc lremove {listVar elemToRemove} {
    upvar 1 $listVar list
    set llen [llength $list]
    for {set i 0} {$i < $llen} {incr i} {
        if {[lindex $list $i] eq $elemToRemove} {
            set list [lreplace $list $i $i]
            return
        }
    }
}
