proc bracketsMatch {input} {
    set brackets [dict create \{ \} \( \) \[ \] ]
    set openers [dict keys $brackets]
    set closers [dict values $brackets]
    set stack [list]

    foreach char [split $input ""] {
        if {$char in $openers} {
            lappend stack $char
        } elseif {$char in $closers} {
            if {
                [llength $stack] == 0 ||
                $char ne [dict get $brackets [lpop stack]]
            } {
                return false
            }
        }
    }

    expr {[llength $stack] == 0}
}


# this will be a core command in Tcl 8.7
# https://core.tcl-lang.org/tips/doc/trunk/tip/523.md
#
proc lpop {listname} {
    upvar 1 $listname lst
    set value [lindex $lst end]
    # this is a performance hack
    # see "Unsharing Objects" in https://wiki.tcl-lang.org/K
    set lst [lrange $lst[set lst {}] 0 end-1]
    return $value
}
