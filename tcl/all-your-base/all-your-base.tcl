proc rebase {inputBase digits outputBase} {
    assert {$inputBase > 1} "input base must be >= 2" 
    assert {$outputBase > 1} "output base must be >= 2" 

    set dec 0
    foreach digit $digits {
        assert {0 <= $digit && $digit < $inputBase} \
            "all digits must satisfy 0 <= d < input base"
        set dec [expr {$dec * $inputBase + $digit}]
    }
    if {$dec == 0} then {return {0}}

    set outDigitsRev {}
    for {} {$dec > 0} {set dec [expr {$dec / $outputBase}]} {
        lappend outDigitsRev [expr {$dec % $outputBase}]
    }
    return [lreverse $outDigitsRev]
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

