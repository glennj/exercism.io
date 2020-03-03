proc recite {items} {
    if {[llength $items] == 0} then {return {}}
    foreachCons pair $items 2 {
        lassign $pair this that
        lappend proverb "For want of a $this the $that was lost."
    }
    lappend proverb "And all for the want of a [lindex $items 0]."
    return $proverb
}


# modelled after Ruby
# https://ruby-doc.org/core-2.6.3/Enumerable.html#method-i-each_cons
#
proc foreachCons {varName list size body} {
    upvar 1 $varName sublist
    coroutine nextCons eachCons $list $size
    while 1 {
        set sublist [nextCons]
        uplevel 1 $body
    }
}

proc eachCons {list size} {
    yield
    set end [expr {[llength $list] - $size}]
    for {set i 0} {$i <= $end} {incr i} {
        yield [lrange $list $i [expr {$i + $size - 1}]]
    }
    # signal the end of the iteration
    yieldto return -level 0 -code break
}
