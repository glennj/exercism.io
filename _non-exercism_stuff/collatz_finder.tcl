#!/usr/bin/env tclsh

set collatzable [dict create 1 1]

set limit [expr {10 * 1000 * 1000}]
puts "limit = $limit"
for {set n 2} {$n < $limit} {incr n} {
    set m $n
    set steps 0
    set found false
    while {$steps < 10000 && $m != 1} {
        incr steps
        set m [expr {$m % 2 == 0 ? $m / 2 : 3 * $m + 1}]
        if {[dict exists $collatzable $m] && [dict get $collatzable $m]} {
            set found true
            break
        }
    }

    if {$found} {
        dict set collatzable $n 1
    } else {
        error "Not a collatz number: $n in steps: $steps ($m)"
    }
}
