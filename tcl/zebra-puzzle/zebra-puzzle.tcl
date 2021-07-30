lappend auto_path .
package require permutations

interp alias {} permutations {} ::permutations::permutationsOfSize {1 2 3 4 5} 5

############################################################
# returns true if "first" is to the right of "second"
proc ::tcl::mathfunc::toTheRightOf {first second} {
    expr {$first == $second + 1}
}

# returns true if "first" is the neighbour of "second"
proc ::tcl::mathfunc::nextTo {first second} {
    expr {toTheRightOf($first, $second) || toTheRightOf($second, $first)}
}

# ref: https://wiki.tcl-lang.org/page/constants
# implement constants as procs
proc FIRST  {} {return 1}
proc MIDDLE {} {return 3}


############################################################
# The clues, and the method that implements them:
#   1. There are five houses.
#   2. The Englishman lives in the red house. (SolveForNationality)
#   3. The Spaniard owns the dog. (SolveForPets)
#   4. Coffee is drunk in the green house. (SolveForBeverages)
#   5. The Ukrainian drinks tea. (SolveForBeverages)
#   6. The green house is immediately to the right of the ivory house.  (SolveForColour)
#   7. The Old Gold smoker owns snails. (SolveForPets)
#   8. Kools are smoked in the yellow house. (SolveForSmokes)
#   9. Milk is drunk in the middle house. (SolveForBeverages)
#  10. The Norwegian lives in the first house. (SolveForNationality)
#  11. The man who smokes Chesterfields lives in the house next to the man with the fox. (SolveForPets)
#  12. Kools are smoked in the house next to the house where the horse is kept.
#  13. The Lucky Strike smoker drinks orange juice. (SolveForSmokes)
#  14. The Japanese smokes Parliaments. (SolveForSmokes)
#  15. The Norwegian lives next to the blue house. (SolveForNationality)
############################################################

oo::class create ZebraPuzzle {
    variable waterDrinker
    variable zebraOwner

    variable one two three four five
    variable red green ivory yellow blue
    variable english spanish ukranian norwegian japanese
    variable dog snails fox horse zebra
    variable oldGold kools chesterfields luckyStrike parliaments
    variable coffee tea milk orangeJuice water

    variable nationalities

    constructor {args} {
        my Solve
    }

    method drinksWater {} {
        return $waterDrinker
    }

    method ownsZebra {} {
        return $zebraOwner
    }

    method Solve {} {
        set waterDrinker "cannot solve puzzle"
        set zebraOwner "cannot solve puzzle"

        foreach p [permutations] {
            if {[my SolveForColour $p]} then break
        }
    }

    method SolveForColour {permutation} {
        lassign $permutation red green ivory yellow blue
        # clue 6
        if {toTheRightOf($green, $ivory)} {
            foreach p [permutations] {
                if {[my SolveForNationality $p]} then {return true}
            }
        }
        return false
    }

    method SolveForNationality {permutation} {
        lassign $permutation english spanish ukranian norwegian japanese
        # clues 2, 10, 15
        if {
            $english == $red
            && $norwegian == [FIRST]
            && nextTo($norwegian, $blue)
        } {
            foreach p [permutations] {
                set nationalities [list \
                    $english   EnglishMan \
                    $spanish   Spaniard   \
                    $ukranian  Ukranian   \
                    $norwegian Norwegian  \
                    $japanese  Japanese   \
                ]

                if {[my SolveForBeverages $p]} then {return true}
            }
        }
        return false
    }

    method SolveForBeverages {permutation} {
        lassign $permutation coffee tea milk orangeJuice water
        # clues 4, 5, 9
        if {
            $coffee == $green
            && $ukranian == $tea
            && $milk == [MIDDLE]
        } {
            foreach p [permutations] {
                if {[my SolveForSmokes $p]} then {return true}
            }
        }
        return false
    }

    method SolveForSmokes {permutation} {
        lassign $permutation oldGold kools chesterfields luckyStrike parliaments
        # clues 8, 13, 14
        if {
            $kools == $yellow
            && $luckyStrike == $orangeJuice
            && $japanese == $parliaments
        } {
            foreach p [permutations] {
                if {[my SolveForPets $p]} then {return true}
            }
        }
        return false
    }

    method SolveForPets {permutation} {
        lassign $permutation dog snails fox horse zebra
        # clues 3, 7, 11, 12
        if {
            $spanish == $dog
            && $oldGold == $snails
            && nextTo($chesterfields, $fox)
            && nextTo($kools, $horse)
        } {
            set waterDrinker [dict get $nationalities $water]
            set zebraOwner   [dict get $nationalities $zebra]
            return true
        }
        return false
    }
}
