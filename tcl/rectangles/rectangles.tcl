oo::class create Rectangles {
    variable grid width height
    variable vertices
    variable count

    constructor {rows} {
        set grid [lmap row $rows {split $row ""}]
        set height [llength $grid]
        set width [llength [lindex $grid end]]
    }

    method count {} {
        my findVertices
        if {[info exists count]} then {return $count}
        set count 0
        foreach tl $vertices {                              ;# top left vertex
            foreach tr [my verticesRightOf $tl] {           ;# top right
                foreach bl [my verticesBelow $tl] {         ;# bottom left
                    set br [Vertex new [$bl row] [$tr col]] ;# bottom right
                    if {[my isVertex $br] &&
                        [my isRectangle $tl $tr $bl $br]
                    } then {
                        incr count
                    }
                }
            }
        }
        return $count
    }

    method findVertices {} {
        if {[info exists vertices]} then return
        set vertices {}
        for {set r 0} {$r < $height} {incr r} {
            for {set c 0} {$c < $width} {incr c} {
                set v [Vertex new $r $c]
                if {[my isVertex $v]} {
                    lappend vertices $v
                }
            }
        }
    }

    method isVertex {v} {
        expr {[lindex $grid [$v row] [$v col]] eq "+"}
    }

    method verticesRightOf {vertex} {
        my vertexSubset $vertex v {
            [$vertex row] == [$v row] &&
            [$vertex col] <  [$v col]
        }
    }

    method verticesBelow {vertex} {
        my vertexSubset $vertex v {
            [$vertex col] == [$v col] &&
            [$vertex row] <  [$v row]
        }
    }

    method vertexSubset {vertex varName expression} {
        set vs {}
        upvar 1 $varName v
        foreach v $vertices {
            if {[uplevel [list expr $expression]]} {
                lappend vs $v
            }
        }
        return $vs
    }
    
    method isRectangle {tl tr bl br} {
        expr {
            [my hasVerticalLine $tl $bl] &&
            [my hasVerticalLine $tr $br] &&
            [my hasHorizontalLine $tl $tr] &&
            [my hasHorizontalLine $bl $br]
        }
    }

    method hasHorizontalLine {left right} {
        set chars [lrange [lindex $grid [$left row]] [$left col] [$right col]]
        regexp {^[+][+-]*[+]$} [join $chars ""]
    }

    method hasVerticalLine {top bottom} {
        set col [$top col]
        set chars [lmap row [lrange $grid [$top row] [$bottom row]] {lindex $row $col}]
        regexp {^[+][+|]*[+]$} [join $chars ""]
    }

    # mark some methods as private
    unexport isVertex
    unexport verticesRightOf
    unexport verticesBelow
    unexport vertexSubset
    unexport isRectangle
    unexport hasHorizontalLine
    unexport hasVerticalLine
}

############################################################
oo::class create Vertex {
    variable r c
    constructor {row col} {
        set r $row
        set c $col
    }
    method row {} {set r}
    method col {} {set c}
    method toList {} {list $r $c}
    method eq {other} {
        expr {$r == [$other row] && $c == [$other col]}
    }
}

############################################################
proc rectangles {input} {
    [Rectangles new $input] count
}
