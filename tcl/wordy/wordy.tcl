proc answer {question} {
    set input [string map {
        "what is"       ""
        "?"             ""
        "plus"          "+"
        "minus"         "-"
        "multiplied by" "*"
        "divided by"    "/"
    } [string tolower $question]]

    # should only be digits, spaces and operators remaining
    assert {[regexp {^[\d\s/*+-]*$} $input]} "unknown operation"

    set expression [regexp -inline -all {\S+} $input]

    # can't be empty
    assert {[llength $expression] > 0} "syntax error"

    while {[llength $expression] > 1} {
        set rest [lassign $expression left op right]

        assert {[string is integer -strict $left]}  "syntax error"
        assert {[string is integer -strict $right]} "syntax error"
        assert {[string match {[-+*/]} $op]}        "syntax error"

        set expression [linsert $rest 0 [expr "$left $op $right"]]
    }

    return $expression
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

