# reuse previous solution
source ../resistor-color-duo/resistor-color-duo.tcl

proc ::resistorColor::label {first second third} {
    set value [expr {[value $first $second] * 10**[colorCode $third]}]

    set prefix ""
    while {[string match {*000} $value]} {
        set prefix [lindex {"" kilo mega giga} [incr idx]]
        regsub {000$} $value "" value
    }

    return "$value ${prefix}ohms"
}
