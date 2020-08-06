if {"Tree" in [info commands]} then return

oo::class create Tree {
    variable value left right

    constructor {input} {
        set value [dict get $input value]

        set left ""
        if {[dict exists $input left]} {
            set leftInput [dict get $input left]
            if {[string_is_dict $leftInput]} {
                set left [Tree new $leftInput]
            }
        }

        set right ""
        if {[dict exists $input right]} {
            set rightInput [dict get $input right]
            if {[string_is_dict $rightInput]} {
                set right [Tree new $rightInput]
            }
        }
    }

    method value {} {return $value}
    method left  {} {return $left}
    method right {} {return $right}

    method setValue {val}  {set value $val}
    method setLeft  {tree} {set left $tree}
    method setRight {tree} {set right $tree}

    method toDict {} {
        set output [dict create]
        dict set output value $value
        if {$left ne ""} {
            dict set output left [$left toDict]
        }
        if {$right ne ""} {
            dict set output right [$right toDict]
        }
        return $output
    }

    method equals {other} {
        return [expr {[my toDict] eq [$other toDict]}]
    }
}

# `string is dict` will be available in Tcl 8.7
# https://www.tcl.tk/man/tcl8.7/TclCmd/string.htm#M11
#
proc string_is_dict {str} {
    expr {
        [string is list -strict $str] &&
        [llength $str] > 0 &&
        [llength $str] % 2 == 0
    }
}
