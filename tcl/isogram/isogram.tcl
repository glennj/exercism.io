interp alias "" isIsogram "" isIsogram_early_return

############################################################
# a few different implementations that would work

proc isIsogram_iterate {input} {
    # remove non-alphabetic chars
    set alphaOnly [string tolower [regsub -all {[^[:alpha:]]} $input ""]]

    set charCount [dict create]
    for {set i 0} {$i < [string length $alphaOnly]} {incr i} {
        dict incr charCount [string range $alphaOnly $i $i]
    }

    return [expr {[dict size $charCount] == [string length $alphaOnly]}]
}

# same as above, but split the string and iterate over that list
proc isIsogram_split {input} {
    set chars [split [string tolower [regsub -all {[^[:alpha:]]} $input ""]] ""]
    set charCount [dict create]
    foreach char $chars {
        dict incr charCount $char
    }
    return [expr {[dict size $charCount] == [llength $chars]}]
}

# extract the alpha chars with regexp
proc isIsogram_regexp {input} {
    set chars [regexp -all -inline {[[:alpha:]]} [string tolower $input]]
    set charCount [dict create]
    foreach char $chars {
        dict incr charCount $char
    }
    return [expr {[dict size $charCount] == [llength $chars]}]
}

# early return
proc isIsogram_early_return {input} {
    set chars [split [string tolower [regsub -all {[^[:alpha:]]} $input ""]] ""]
    set seen [dict create]
    foreach char $chars {
        if {[dict exists $seen $char]} then {return false}
        dict set seen $char true
    }
    return true
}

# test for alpha-ness with `string is` not with `regexp`
proc isIsogram_non_regex {input} {
    set seen [dict create]
    foreach char [split [string toupper $input] ""] {
        if {![string is alpha $char]} then continue
        if {[dict exists $seen $char]} then {return false}
        dict set seen $char 1
    }
    return true
}

############################################################
proc benchmark {} {
    set procs {
        isIsogram_iterate
        isIsogram_split
        isIsogram_regexp
        isIsogram_early_return
        isIsogram_non_regex
    }
    set alphabet  abcdefghijklmnopqrstuvwxyz
    set dup_end "${alphabet}A"
    set dup_mid abcdefghijklmMnopqrstuvwxyz
    set dup_start "A$alphabet"

    # validate the procs work
    proc assert {condition errMsg} {
        if {![uplevel 1 [list expr $condition]]} {
            error $errMsg
        }
    }
    foreach proc $procs {
        assert { [$proc $alphabet]}  "wrong result for $proc $alphabet"
        assert {![$proc $dup_end]} "wrong result for $proc $dup_end"
        assert {![$proc $dup_mid]} "wrong result for $proc $dup_mid"
        assert {![$proc $dup_start]} "wrong result for $proc $dup_start"
    }

    # benchmarking
    set w [tcl::mathfunc::max {*}[lmap p $procs {string length $p}]]
    proc dotest {p a case} {
        upvar w w
        set t [time {$p $a} 100000]
        set units [lassign $t sec]
        puts [format {%-*s  %-12s  %10.6f %s} $w $p $case $sec $units]
    }
    foreach proc $procs {
        dotest $proc $alphabet  "is isogram"
        dotest $proc $dup_end "dup at end"
        dotest $proc $dup_mid "dup at mid"
        dotest $proc $dup_start "dup at start"
        puts ""
    }
}

if {$argv0 eq [info script]} then benchmark
