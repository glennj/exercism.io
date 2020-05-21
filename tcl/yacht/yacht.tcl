source dict.tcl

oo::class create Yacht {
    variable dice groups sum

    constructor {theDice} {
        set dice $theDice
        foreach die $dice {
            dict incr groups $die
            incr sum $die
        }
    }

    method score {category} { my $category $dice }

    method unknown {args} { error "Invalid category" }

    method choice {} { return $sum }

    method yacht {} {
        expr {[dict size $groups] == 1 ? 50 : 0}
    }

    method num {n} {
        expr {[dict getdef $groups $n 0] * $n}
    }
    forward ones    my num 1
    forward twos    my num 2
    forward threes  my num 3
    forward fours   my num 4
    forward fives   my num 5
    forward sixes   my num 6

    method "full house" {} {
        set groupings [lsort [dict values $groups]]
        expr {$groupings eq "2 3" ? $sum : 0}
    }

    method "four of a kind" {} {
        dict for {n count} $groups {
            if {$count in {4 5}} {
                return [expr {4 * $n}]
            }
        }
        return 0
    }

    method straight {start} {
        set ds [lsort $dice]
        expr {
            ( ($ds eq "1 2 3 4 5" && $start == 1) ||
              ($ds eq "2 3 4 5 6" && $start == 2)
            ) ? 30 : 0
        }
    }
    forward "little straight"  my straight 1
    forward "big straight"     my straight 2
}


proc score {dice category} {
    [Yacht new $dice] $category
}
