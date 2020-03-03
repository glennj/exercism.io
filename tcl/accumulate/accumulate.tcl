proc accumulate {varname values body} {
    upvar 1 $varname elem

    # Tcl builtin
    # lmap elem $values {uplevel 1 $body}

    set result [list]
    foreach elem $values {
        lappend result [uplevel 1 $body]
    }
    return $result
}
