oo::class create Domino {
    variable dots
    constructor {left right} {
        set dots [list $left $right]
    }

    method left  {} {lindex $dots 0}
    method right {} {lindex $dots end}
    method toString {} {format {[%d|%d]} {*}$dots}

    method has? {dot} {expr {$dot in $dots}}

    method reverse {} {
        set dots [lreverse $dots]
        self
    }
}


oo::class create Dominoes {
    variable dominoes
    constructor {listOfDominoes} {
        set dominoes $listOfDominoes
    }

    method size {} {llength $dominoes}

    method toString {} {
        format {%d dominoes: %s} [my size] \
            [join [lmap d $dominoes {$d toString}]]
    }

    method hasChain? {} {
        if {[my size] == 0} {
            return true
        }

        for {set i 0} {$i < [llength $dominoes]} {incr i} {
            set d [lindex $dominoes $i]
            set remaining [lreplace $dominoes $i $i]

            set chain [my buildChain $d $remaining]
            if {[llength $chain] == 0} {
                set chain [my buildChain [$d reverse] $remaining]
            }

            if {[llength $chain] != 0} {
                return true
            }
        }

        return false
    }

    method buildChain {chain available} {
        set tail [[lindex $chain end] right]

        if {[llength $available] == 0} {
            set head [[lindex $chain 0] left]
            if {$head == $tail} {
                return $chain
            } else {
                return {}
            }
        }

        for {set i 0} {$i < [llength $available]} {incr i} {
            set d [lindex $available $i]
            if {[$d has? $tail]} {
                if {[$d left] != $tail} then {$d reverse}

                set newChain [my buildChain \
                    [concat $chain $d] \
                    [lreplace $available $i $i] \
                ]

                if {[llength $newChain] != 0} {
                    return $newChain
                }
            }
        }

        return {} ;# no chain detected
    }
    unexport buildChain ;# private
}


proc dominoChain {list} {
    set ds [lmap dots $list {Domino new {*}$dots}]
    [Dominoes new $ds] hasChain?
}
