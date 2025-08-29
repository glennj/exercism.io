# The goal is to determine the rows and columns.
# From these, all the other elements can be found.
#
# If the given information is insufficient to get rows/cols
# then find all combinations of rows&cols (up to an arbitrary max size)
# 
# Then we see how many of the potential solutions match the given criteria.

proc maxSize {} {return 1000}

proc jigsawData {input} {
    set possibles [list]

    if {[dict exists $input pieces]} {
        set pieces [dict get $input pieces]
        for {set rows 1} {$rows <= $pieces} {incr rows} {
            if {$pieces % $rows == 0} {
                set data [allData $rows [expr {$pieces / $rows}]]
                if {[matchesCriteria $data $input]} {
                    lappend possibles $data
                }
            }
        }

    } elseif {[dict exists $input inside]} {
        set inside [dict get $input inside]
        for {set irows 1} {$irows <= $inside} {incr irows} {
            if {$inside % $irows == 0} {
                set data [allData [expr {$irows + 2}] [expr {$inside / $irows + 2}]]
                if {[matchesCriteria $data $input]} {
                    lappend possibles $data
                }
            }
        }

    } else {
        for {set rows [maxSize]} {$rows > 0} {incr rows -1} {
            for {set cols [maxSize]} {$cols > 0} {incr cols -1} {
                set data [allData $rows $cols]
                if {[matchesCriteria $data $input]} {
                    lappend possibles $data
                }
            }
        }
    }

    switch -- [llength $possibles] {
        1       {lindex $possibles 0}
        0       {error "Contradictory data"}
        default {error "Insufficient data"}
    }
}

proc allData {rows cols} {
    set pieces [expr {$rows * $cols}]
    set inside [expr {($rows - 2) * ($cols - 2)}]
    set aspect [expr {1.0 * $cols / $rows}]

    return [list \
        pieces      $pieces \
        border      [expr {$pieces - $inside}] \
        inside      $inside \
        rows        $rows \
        columns     $cols \
        aspectRatio $aspect \
        format      [expr {$aspect == 1 ? "square" : ($aspect < 1 ? "portrait" : "landscape")}] \
    ]
}

proc matchesCriteria {data criteria} {
    dict for {key value} $data {
        if {[dict exists $criteria $key] && $value != [dict get $criteria $key]} {
            return false
        }
    }
    return true
}
