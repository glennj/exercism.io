proc largestSeriesProduct {digits span} {
    assert {$span <= [string length $digits]} "span must not exceed string length"
    assert {![regexp {\D} $digits]} "digits input must only contain digits"
    assert {$span >= 0} "span must not be negative"

    set max -Inf
    set endIdx [expr {[string length $digits] - $span}]

    for {set i 0; set j $span} {$i <= $endIdx} {incr i; incr j} {
        set series [split [string range $digits $i $j-1] ""]
        set product [::tcl::mathop::* {*}$series]
        set max [expr {max($max, $product)}]
    }
    return $max
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
