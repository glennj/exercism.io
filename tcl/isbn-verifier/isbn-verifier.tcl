proc isValid {isbn} {
    # hyphens are only valid non-digit characters
    regsub -all {[-]} $isbn "" num
    if {![regexp {^\d{9}[\dX]$} $num]} {
        return false
    }

    set digits [lmap digit [split $num ""] {
        expr {$digit eq "X" ? 10 : $digit}
    }]

    set m 11
    foreach digit $digits {
        incr sum [expr {$digit * [incr m -1]}]
    }
    expr {$sum % 11 == 0}
}
