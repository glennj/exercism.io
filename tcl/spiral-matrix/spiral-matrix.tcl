proc spiralMatrix {n} {
    if {$n == 0} then return

    set matrix [lrepeat $n [lrepeat $n NaN]]

    set x 0
    set y 0

    cycle nextDelta {0 1} {1 0} {0 -1} {-1 0}
    lassign [nextDelta] dx dy

    for {set i 1} {$i <= $n**2} {incr i} {
        lset matrix $x $y $i

        if {
            $x + $dx < 0 || $x + $dx == $n ||
            $y + $dy < 0 || $y + $dy == $n ||
            [string is integer [lindex $matrix $x+$dx $y+$dy]]
        } {
            lassign [nextDelta] dx dy
        }
        incr x $dx
        incr y $dy
    }

    rename nextDelta ""
    return $matrix
}

# Inspired by ruby
# https://ruby-doc.org/core-2.6.3/Array.html#method-i-cycle
#
proc cycle {cmdName args} {
    coroutine $cmdName apply {{elements} {
        yield [info coroutine]
        set idx 0
        set len [llength $elements]
        while {1} {
            yield [lindex $elements $idx]
            set idx [expr {($idx + 1) % $len}]
        }
    }} $args
}
