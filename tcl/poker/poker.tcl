oo::class create Card {
    variable face suit value

    constructor {input} {
        regexp {(.{1,2})(.)} $input -> face suit
        switch -exact $face {
            A {set value 14}
            K {set value 13}
            Q {set value 12}
            J {set value 11}
            default {set value $face}
        }
    }

    method value    {} {return $value}
    method suit     {} {return $suit}
    method toString {} {return "$face$suit"}
}


############################################################
oo::class create PokerHand {
    variable category rank
    variable cards cardsSorted groupings
    variable originalInput

    constructor {input} {
        set originalInput $input
        foreach card $input {
            lappend cards [Card new $card]
        }
        my evaluateHand     ;# set the category and rank
    }

    method compareCards {a b} {
        if {[$a value] > [$b value]} then {return +1}
        if {[$a value] < [$b value]} then {return -1}
        return 0
    }

    method evaluateHand {} {
        set cardsSorted [lsort -decreasing -command {my compareCards} $cards]
        set groupings [my groupCards]

        if {[my is5ofaKind     ]} then return
        if {[my isStraightFlush]} then return
        if {[my is4ofaKind     ]} then return
        if {[my isFullHouse    ]} then return
        if {[my isFlush        ]} then return
        if {[my isStraight     ]} then return
        if {[my is3ofaKind     ]} then return
        if {[my is2Pair        ]} then return
        if {[my is1Pair        ]} then return

        set category 9
        set rank [my allCardsRank]
    }

    method category {} {return $category}
    method rank     {} {return $rank}
    method toString {} {return $originalInput}

    method allCardsRank {} {
        set rank 0
        foreach card $cardsSorted {
            set rank [expr {14 * $rank + [$card value]}]
        }
        return $rank
    }

    method isStraightFlush {} {
        if {[my isFlush] && [my isStraight]} {
            set category 1
            # rank set by isStraight
            return yes
        }
        return no
    }

    method isStraight {} {
        set values [lmap card $cardsSorted {$card value}]
        if {$values eq "14 5 4 3 2"} {
            set category 5
            set rank 5
            return yes
        }
        set straight yes
        for {set i 1} {$i < [llength $values]} {incr i} {
            if {[lindex $values $i-1] - [lindex $values $i] != 1} {
                set straight no
                break
            }
        }
        if {$straight} {
            set category 5
            set rank [lindex $values 0]
        }
        return $straight
    }

    method isFlush {} {
        foreach card $cards {
            dict incr countSuits [$card suit]
        }
        if {[dict size $countSuits] == 1} {
            set category 4
            set rank [my allCardsRank]
            return yes
        }
        return no
    }

    forward is5ofaKind   my isGrouping "5"       1
    forward is4ofaKind   my isGrouping "4 1"     2
    forward isFullHouse  my isGrouping "3 2"     3
    forward is3ofaKind   my isGrouping "3 1 1"   6
    forward is2Pair      my isGrouping "2 2 1"   7
    forward is1Pair      my isGrouping "2 1 1 1" 8

    method groupCards {} {
        foreach card $cards {
            dict incr groupings [$card value]
        }
        # sort by value
        set groupings [lsort -integer -decreasing -stride 2 -index 0 $groupings]
        # then sort by group count
        set groupings [lsort -integer -decreasing -stride 2 -index 1 $groupings]
    }

    method isGrouping {groups catgry} {
        if {$groups eq [dict values $groupings]} {
            set category $catgry
            set rank 0
            dict for {value _} $groupings {
                set rank [expr {14 * $rank + $value}]
            }
            return yes
        }
        return no
    }
}

############################################################
oo::class create PokerGame {
    variable hands

    constructor {input} {
        set hands [lmap hand $input {PokerHand new $hand}]
    }

    method winningHands {} {
        set winningHands {}

        set minCategory Inf
        foreach hand $hands {
            set cat [$hand category]
            if {$cat < $minCategory} {
                set minCategory $cat
                set winningHands [list $hand]
            } elseif {$cat == $minCategory} {
                lappend winningHands $hand
            }
        }

        if {[llength $winningHands] == 1} {
            set reallyWinningHands $winningHands
        } else {
            set reallyWinningHands {}
            set maxRank -Inf
            foreach hand $winningHands {
                set rank [$hand rank]
                if {$rank > $maxRank} {
                    set maxRank $rank
                    set reallyWinningHands [list $hand]
                } elseif {$rank == $maxRank} {
                    lappend reallyWinningHands $hand
                }
            }
        }
        lmap hand $reallyWinningHands {$hand toString}
    }
}

############################################################
proc bestHands {hands} {
    [PokerGame new $hands] winningHands
}
