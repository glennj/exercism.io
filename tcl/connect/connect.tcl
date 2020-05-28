############################################################
oo::class create Connect {
    variable matrix height width
    variable board

    constructor {board} {
        set matrix [lmap str $board {regexp -all -inline {\S} $str}]
        set height [llength $matrix]
        set width  [llength [lindex $matrix 0]]
    }

    method winner {} {
        set board $matrix
        if {[my isWinner "O"]} then {return "O"}

        set board [transpose $matrix]
        if {[my isWinner "X"]} then {return "X"}
    }

    method isWinner {player} {
        # Player's tokens in first row
        set stack {}
        for {set i 0} {$i < $width} {incr i} {
            if {[lindex $board 0 $i] eq $player} {
                lappend stack [list 0 $i]
            }
        }
        if {[llength $stack] == 0} then {return false}
        if {[llength $board] == 1} then {return true}

        # find a path to the last row
        set last_row [expr {[llength $board] - 1}]
        while {[llength $stack] > 0} {
            lassign [lpop stack] row col
            foreach coords [my neighbours $player $row $col] {
                lassign $coords rr cc
                if {$rr == $last_row} then {return true}
                lset board $row $col "visited"
                lappend stack $coords
            }
        }
        return false
    }

    method neighbours {player row col} {
        set neighbours {}
        foreach dr {-1 0 1} {
            set rr [expr {$row + $dr}]
            foreach dc {-1 0 1} {
                set cc [expr {$col + $dc}]
                if {
                    $dr != $dc &&
                    (0 <= $rr && $rr < $height) &&
                    (0 <= $cc && $cc < $width) &&
                    [lindex $board $rr $cc] eq $player
                } then {
                    lappend neighbours [list $rr $cc]
                }
            }
        }
        return $neighbours
    }
}


############################################################
# Utility procs
#
# ref: https://wiki.tcl-lang.org/page/K#c2a6014c2d129837889d8a8000d05e5c3b44e8f6b46cab777c04df8a927bfad2
proc K {x y} {return $x}

proc lpop {listName} {
    upvar 1 $listName list
    set elem [lindex $list end]
    set list [lreplace [K $list [set list ""]] end end]
    return $elem
}

proc transpose {matrix} {
    set transposed {}
    for {set i 0} {$i < [llength [lindex $matrix 0]]} {incr i} {
        lappend transposed [lmap row $matrix {lindex $row $i}]
    }
    return $transposed
}


############################################################
proc winner {board} {
    return [[Connect new $board] winner]
}
