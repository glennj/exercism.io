proc isValid {isbn} {
    regsub -all {[^\dX]} $isbn "" num
    if {![regexp {^\d{9}[\dX]$} $num]} {
        return false
    }
    set m 11
    foreach digit [split $num ""] {
        incr sum [expr {($digit eq "X" ? 10 : $digit) * [incr m -1]}]
    }
    expr {$sum % 11 == 0}
}
