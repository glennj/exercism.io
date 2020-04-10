proc recite {from to} {
    set items {
        {"house that Jack built"            ""}
        {"malt"                             "lay in"}
        {"rat"                              "ate"}
        {"cat"                              "killed"}
        {"dog"                              "worried"}
        {"cow with the crumpled horn"       "tossed"}
        {"maiden all forlorn"               "milked"}
        {"man all tattered and torn"        "kissed"}
        {"priest all shaven and shorn"      "married"}
        {"rooster that crowed in the morn"  "woke"}
        {"farmer sowing his corn"           "kept"}
        {"horse and the hound and the horn" "belonged to"}
    }

    set verses {}

    for {set i [expr {$from - 1}]} {$i < $to} {incr i} {
        set verse "This is the [lindex $items $i 0]"

        for {set j [expr {$i - 1}]} {$j >= 0} {incr j -1} {
            append verse " that " [lindex $items $j+1 1]
            append verse " the "  [lindex $items $j   0]
        }

        lappend verses "${verse}."
    }

    return [join $verses \n]
}
