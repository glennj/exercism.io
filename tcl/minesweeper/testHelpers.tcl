#############################################################
# Override some tcltest procs with additional functionality

# Allow an environment variable to override `skip`
proc skip {patternList} {
    if { [info exists ::env(RUN_ALL)]
         && [string is boolean -strict $::env(RUN_ALL)]
         && $::env(RUN_ALL)
    } then return else {
        uplevel 1 [list ::tcltest::skip $patternList]
    }
}

# Exit non-zero if any tests fail.
# The cleanupTests resets the numTests array, so capture it first.
proc cleanupTests {} {
    set failed [expr {$::tcltest::numTests(Failed) > 0}]
    uplevel 1 ::tcltest::cleanupTests
    if {$failed} then {exit 1}
}

#############################################################
# Some procs that are handy for Tcl test custom matching.
# ref http://www.tcl-lang.org/man/tcl8.6/TclCmd/tcltest.htm#M20


# Compare two ordered lists without comparing the lists themselves 
# as strings.
# e.g.
#     set first {
#         a  b  c
#     }
#     set second [list a b c]
#     expr {$first eq $second}           ;# 0
#     expr {$first == $second}           ;# 0
#     orderedListsMatch $first $second   ;# true
#
proc orderedListsMatch {expected actual} {
    if {[llength $expected] != [llength $actual]} {
        return false
    }
    foreach e $expected a $actual {
        if {$e != $a} {
            return false
        }
    }
    return true
}
customMatch orderedLists orderedListsMatch


# two lists have the same elements, in no particular order
proc unorderedListsMatch {expected actual} {
    if {[llength $expected] != [llength $actual]} {
        return false
    }
    foreach elem $expected {
        if {[lsearch -exact $actual $elem] == -1} {
            return false
        }
    }
    return true
}
customMatch unorderedLists unorderedListsMatch


# The expected value is one of a list of values.
proc inListMatch {expectedList actual} {
    return [expr {$actual in $expectedList}]
}
customMatch inList inListMatch


# Compare floating point numbers 
proc floatMatch {expected actual {epsilon 1e-6}} {
    return [expr {abs($expected - $actual) <= $epsilon}]
}
customMatch float floatMatch


# Compare a list of floating point numbers 
proc listOfFloatsMatch {expected actual} {
    foreach e $expected a $actual {
        if {![floatMatch $e $a]} {
            return false
        }
    }
    return true
}

#############################################################
# Convenience function to set the precision of a real number.
proc roundTo {precision number} {
    return [format {%.*f} $precision $number]
}
