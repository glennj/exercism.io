namespace eval dnd {
    namespace export character ability modifier
    namespace ensemble create

    variable characteristics {
        strength
        dexterity
        constitution
        intelligence
        wisdom
        charisma
    }

    proc modifier {score} {
        # floor() rounds towards -Inf
        expr {int(floor(($score - 10) / 2.0))}
    }

    proc d6 {} {
        expr {1 + int(6 * rand())}
    }

    proc ability {} {
        set dice [list [d6] [d6] [d6] [d6]]
        set sum [::tcl::mathop::+ {*}$dice]
        set min [::tcl::mathfunc::min {*}$dice]
        expr {$sum - $min}
    }

    proc hitpoints {constitution} {
        expr {10 + [modifier $constitution]}
    }

    proc character {} {
        variable characteristics
        set character [dict create]
        foreach c $characteristics {
            dict set character $c [ability]
        }
        dict set character hitpoints \
            [hitpoints [dict get $character constitution]]
        return $character
    }
}
