# reuse the previous solution
source ../resistor-color/resistor-color.tcl

proc ::resistorColor::value {args} {
    return [join [lmap c [lrange $args 0 1] {colorCode $c}] ""]
}
