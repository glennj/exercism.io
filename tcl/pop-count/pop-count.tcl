proc eggCount {number} {
    # as a gnarly one-liner
    #tcl::mathop::+ {*}[split [format %b $number] ""]

    # looping
    set count 0
    while {$number > 0} {
        incr count [expr {$number & 1}]
        set number [expr {$number >> 1}]
    }
    return $count
}
