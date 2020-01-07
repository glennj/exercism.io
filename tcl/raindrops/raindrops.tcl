proc raindrops {number} {
    dict for {divisor sound} {3 Pling 5 Plang 7 Plong} {
        if {$number % $divisor == 0} {
            append drops $sound
        }
    }
    setIfUnset drops $number
    return $drops
}

proc setIfUnset {varname value} {
    upvar 1 $varname var
    if {![info exists var]} {
        set var $value
    }
}
