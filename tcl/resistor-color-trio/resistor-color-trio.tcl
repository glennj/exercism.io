# reuse previous solution
source ./resistor-color-duo.tcl

proc ::resistorColor::label {first second third} {
    set value [expr {[value $first $second] * 10**[colorCode $third]}]

    set idx 0
    while {[string match {*000} $value]} {
        incr idx
        regsub {000$} $value "" value
    }
    set prefix [lindex {"" kilo mega giga} $idx]

    return "$value ${prefix}ohms"
}
