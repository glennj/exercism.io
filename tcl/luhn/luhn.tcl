proc luhn {digits} {
    set digits [regsub -all " " $digits ""]
    if {[string length $digits] <= 1 || [regexp {\D} $digits]} {
        return false
    }

    set double false
    foreach digit [lreverse [split $digits ""]] {
        if {$double} {
            set digit [lindex {0 2 4 6 8 1 3 5 7 9} $digit]
        }
        incr sum $digit
        set double [expr {!$double}]
    }
 
    return [expr {$sum % 10 == 0}]
}
