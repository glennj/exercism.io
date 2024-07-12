package provide structures::range 0.1

package require Tcl 8.6-

# this is "half-inclusive"

oo::class create Range {
    variable low
    variable high
    variable current

    constructor {args} {
        switch -exact [llength $args] {
            2 { lassign $args low high }
            1 { lassign [list 0 {*}$args] low high }
            default { error "help here" }
        }
        my reset
    }

    method reset {} {
        set current $low
    }

    method current {} {
        return $current
    }

    method tolist {} {
        set list [list]
        for {set i $low} {$i < $high} {incr i} {
            lappend list $i
        }
        return $list
    }

    method next {} {
        set value $current
        if {$value == $high} {
            return -code break $value
        }
        incr current
        return $value
    }

    method yieldNext {} {
        yield
        for {set i $low} {$i < $high} {incr i} {
            yield $i
        }
        yieldto return -level 0 -code break
    }

    method for {varname body} {
        upvar 1 $varname var
        set coro [clock clicks]
        coroutine $coro my yieldNext
        while 1 {
            set var [$coro]
            uplevel 1 $body
        }
    }

    method map {varname body} {
        upvar 1 $varname var
        set result [list]
        my for var {lappend result [uplevel 1 $body]}
        return $result
    }
}
