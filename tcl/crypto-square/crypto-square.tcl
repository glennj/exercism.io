proc encrypt {plaintext} {
    set input [regsub -all {[^[:alnum:]]} [string tolower $plaintext] ""]
    if {$input eq ""} then {return ""}

    set segmentLength [expr {int(ceil(sqrt([string length $input])))}]
    set segments [regexp -all -inline ".{1,$segmentLength}" $input]
    # pad the last element with spaces
    lset segments end [format {%-*s} $segmentLength [lindex $segments end]]
    set transposed {}
    for {set i 0} {$i < $segmentLength} {incr i} {
        lappend transposed [join [lmap s $segments {string index $s $i}] ""]
    }
    return [join $transposed " "]
}
