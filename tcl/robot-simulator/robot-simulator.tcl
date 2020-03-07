oo::class create Robot {
    variable position

    constructor {{initialPos {}}} {
        set position [dict merge {x 0 y 0 direction north} $initialPos]

        dict with position {
            assert {$direction in {north east south west}} "invalid direction"
            assert {[string is integer -strict $x]} "invalid x coordinate"
            assert {[string is integer -strict $y]} "invalid y coordinate"
        }
    }

    method position {} {
        return $position
    }

    method move {instructions} {
        foreach inst [split $instructions ""] {
            switch -- $inst {
                A       {my advance}
                L - R   {my turn $inst}
                default {error "invalid instruction: $inst"}
            }
        }
    }

    method advance {} {
        set delta {
            north {X  0  Y  1}
            east  {X  1  Y  0}
            south {X  0  Y -1}
            west  {X -1  Y  0}
        }
        dict update position x xPos y yPos direction dir {
            dict with delta $dir {
                incr xPos $X
                incr yPos $Y
            }
        }
    }
    unexport advance    ;# private method

    method turn {which} {
        set turn {
            north {L west  R east}
            east  {L north R south}
            south {L east  R west}
            west  {L south R north}
        }
        dict update position direction dir {
            set dir [dict get $turn $dir $which]
        }
    }
    unexport turn   ;# private method
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

