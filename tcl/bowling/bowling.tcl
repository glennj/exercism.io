oo::class create Bowling {

    variable frame
    variable score
    variable bonuses
    variable currentFrame

    constructor {} {
        set frame 1
        set score 0
        set bonuses {}
        set currentFrame {}
    }

    method roll {pins} {
        foreach roll $pins {
            assert {$roll  >=  0} "Negative roll is invalid"
            assert {$roll  <= 10} "Pin count exceeds pins on the lane"
            assert {$frame <= 10} "Cannot roll after game is over"

            incr score $roll
            my applyBonuses $roll
            lappend currentFrame $roll

            if {$frame < 10} {
                my countNormalFrame
            } else {
                my countTenthFrame
            }
        }
    }

    method applyBonuses {roll} {
        foreachWithIndex {i bonus} $bonuses {
            if {$bonus > 0} {
                incr score $roll
                lset bonuses $i [expr {$bonus - 1}]
            }
        }
    }

    method nextFrame {} {
        incr frame
        set currentFrame {}
    }

    method countNormalFrame {} {
        set n [llength $currentFrame]
        lassign $currentFrame a b

        if {$a == 10} {
            lappend bonuses 2
            my nextFrame
        } elseif {$n == 2} {
            assert {$a + $b <= 10} "Pin count exceeds pins on the lane"
            if {$a + $b == 10} {
                lappend bonuses 1
            }
            my nextFrame
        }
    }

    method countTenthFrame {} {
        set n [llength $currentFrame]
        if {$n == 1} then return

        lassign $currentFrame a b c
        if {$a == 10} {
            if {$n == 3} {
                assert {$b == 10 || $b + $c <= 10} "Pin count exceeds pins on the lane"
                my nextFrame
            }
        } else {
            assert {$a + $b <= 10} "Pin count exceeds pins on the lane"
            if {$a + $b < 10 || $n == 3} {
                my nextFrame
            }
        }
    }

    method score {} {
        assert {$frame == 11} "Score cannot be taken until the end of the game"
        return $score
    }
}

############################################################
proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

proc foreachWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set len [llength $list]
    for {set idx 0} {$idx < $len} {incr idx} {
        set elem [lindex $list $idx]
        uplevel 1 $body
    }
}
