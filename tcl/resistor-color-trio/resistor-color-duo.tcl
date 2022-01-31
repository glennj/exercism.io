# reuse the previous solution
source ./resistor-color.tcl

proc ::resistorColor::value {args} {
    set value 0
    foreach color [lrange $args 0 1] {
        set value [expr {10 * $value + [colorCode $color]}]
    }
    return $value
}
