# translation of Java reference solution
# https://github.com/exercism/java/blob/master/exercises/go-counting/.meta/src/reference/java/GoCounting.java

lappend auto_path ../lib
package require stack

proc all {elemVar elements condition} {
    upvar 1 $elemVar elem
    foreach elem $elements {
        if {![uplevel 1 [list expr $condition]]} {
            return false
        }
    }
    return true
}

oo::class create GoBoard {
    variable board height width
    variable NoStone Stones Players

    constructor {input} {
        set board [lmap line $input {split $line ""}]
        set height [llength $board]
        set width [llength [lindex $board 0]]

        set NoStone " "
        set Players {B black W white}
        set Stones {B W " "}
    }

    method Get {point} {
        lassign $point x y
        lindex $board $y $x
    }

    method Occupied {point} {
        expr {[my Get $point] in [dict keys $Players]}
    }

    method Valid {point} {
        lassign $point x y
        expr {(0 <= $x && $x < $width) && (0 <= $y && $y < $height)}
    }

    method Adjacent {point} {
        lassign $point x y
        lmap {dx dy} {0 1  0 -1  1 0  -1 0} {
            set p [list [expr {$x + $dx}] [expr {$y + $dy}]]
            if {![my Valid $p]} then continue
            set p
        }
    }

    method territory {point} {
        if {![my Valid $point]} {
            error "Invalid coordinate"
        }
        if {[my Occupied $point]} {
            return {none {}}
        }

        set visited {}
        set border {}
        set territory {}

        set tovisit [Stack new]
        $tovisit push $point

        while {![$tovisit isEmpty]} {
            set current [$tovisit pop]
            set stone [my Get $current]
            if {$stone eq $NoStone} {
                lappend visited $current
                lappend territory $current
                foreach point [my Adjacent $current] {
                    if {$point ni $visited && $point ni $territory} {
                        $tovisit push $point
                    }
                }
            } else {
                lappend border $stone
            }
        }

        if {[llength $border] == 0} {
            return {none {}}
        }
        set border [lassign $border owner]
        if {[all stone $border {$stone eq $owner}]} {
            set ownerName [dict get $Players $owner]
        } else {
            set ownerName none
        }
        return [list $ownerName [lsort $territory]]
    }

    method territories {} {
        set territories [dict create black {} white {} none {}]
        set seen [list]
        for {set x 0} {$x < $width} {incr x} {
            for {set y 0} {$y < $height} {incr y} {
                set p [list $x $y]
                if {$p in $seen} then continue

                # stones on the board are not part of any territory
                if {[my Get $p] eq $NoStone} {
                    lassign [my territory $p] owner points
                    if {$owner eq "none"} {
                        lappend points $p
                    }
                    dict lappend territories $owner {*}$points
                    lappend seen {*}$points
                }
            }
        }
        return $territories
    }
}
