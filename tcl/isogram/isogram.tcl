proc isIsogram {input} {
    # remove non-alphabetic chars
    set input [string tolower [regsub -all {[^[:alpha:]]} $input ""]]

    set chars [dict create]
    for {set i 0} {$i < [string length $input]} {incr i} {
        dict incr chars [string range $input $i $i]
    }

    return [expr {[dict size $chars] == [string length $input]}]
}
