#!/usr/bin/env tclsh

proc findSequence {input} {
    set line [dict get $input start]
    set prisms [dict get $input prisms]

    set sequence []

    while {true} {
        dict update line angle a theta th {
            set a  [expr {fmod(fmod($a, 360) + 360, 360)}]
            set th [expr {rad($a)}]
        }

        set inline [lmap p $prisms {expr {
            [isInline $p $line] ? $p : [continue]
        }}]
        set p [nextPrism $inline $line]
        if {$p == ""} break

        lappend sequence [dict get $p id]
        dict update line x lx y ly angle la {
            dict with p {
                set lx $x
                set ly $y
                set la [expr {$la + $angle}]
            }
        }
    }
    return $sequence
}

proc lselect {lst predicate} {
    lmap elem $lt {expr {[$predicate $elem] ? $elem : [continue]}}
}

proc isInline {prism line} {
    # find the perpendicular distance of the point from the line
    set dx [expr {[dict get $prism x] - [dict get $line x]}]
    set dy [expr {[dict get $prism y] - [dict get $line y]}]
    set theta [dict get $line theta]
    set dist [expr {$dx * sin($theta) - $dy * cos($theta)}]

    set eps 0.0011 ;# value found by trial and error
    expr {abs($dist) <= $eps}
}

proc nextPrism {prisms line} {
    set next ""
    set min [expr {(1<<64) - 1}]
    foreach p $prisms {
        set dx [expr {[dict get $p x] - [dict get $line x]}]
        set dy [expr {[dict get $p y] - [dict get $line y]}]
        set theta [dict get $line theta]
        set projection [expr {$dx * cos($theta) + $dy * sin($theta)}]
        if {$projection > 0 && $projection < $min} {
            set min $projection
            set next $p
        }
    }
    return $next
}

proc ::tcl::mathfunc::rad {deg} {
    expr {$deg * 4.0 * atan(1) / 180.0}
}
