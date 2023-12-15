proc clean {input} {
    # remove valid non-digits
    regsub -all {[-+.()\s]} $input "" clean

    # so many assertions...

    assert {![regexp {[[:alpha:]]} $clean]} "letters not permitted"
    assert {![regexp {\D} $clean]} "punctuations not permitted"

    assert {[string length $clean] >= 10} "must not be fewer than 10 digits"
    assert {[string length $clean] <= 11} "must not be greater than 11 digits"
    # remove country code if present
    if {[string length $clean] == 11 && [regsub {^1} $clean "" clean] != 1} {
        error "11 digits must start with 1"
    }

    assert {![string match {0*} $clean]} "area code cannot start with zero"
    assert {![string match {1*} $clean]} "area code cannot start with one"
    assert {![string match {???0*} $clean]} "exchange code cannot start with zero"
    assert {![string match {???1*} $clean]} "exchange code cannot start with one"

    return $clean
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}


