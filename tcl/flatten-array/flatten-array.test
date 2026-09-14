#!/usr/bin/env tclsh
# generated: 2026-07-17T20:35:35Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "flatten-array.tcl"


# Note that Tcl does not have a "null" value.
# Typically the empty string is used instead.

test flatten-array-1 "empty" -body {
    flatten {}
} -returnCodes ok -result {}

skip flatten-array-2
test flatten-array-2 "no nesting" -body {
    flatten {0 1 2}
} -returnCodes ok -result {0 1 2}

skip flatten-array-3
test flatten-array-3 "flattens a nested array" -body {
    flatten {{{}}}
} -returnCodes ok -result {}

skip flatten-array-4
test flatten-array-4 "flattens array with just integers present" -body {
    flatten {1 {2 3 4 5 6 7} 8}
} -returnCodes ok -result {1 2 3 4 5 6 7 8}

skip flatten-array-5
test flatten-array-5 "5 level nesting" -body {
    flatten {0 2 {{2 3} 8 100 4 50} -2}
} -returnCodes ok -result {0 2 2 3 8 100 4 50 -2}

skip flatten-array-6
test flatten-array-6 "6 level nesting" -body {
    flatten {1 {2 3 {4 5} 6 7} 8}
} -returnCodes ok -result {1 2 3 4 5 6 7 8}

skip flatten-array-7
test flatten-array-7 "null values are omitted from the final result" -body {
    flatten {1 2 ""}
} -returnCodes ok -result {1 2}

skip flatten-array-8
test flatten-array-8 "consecutive null values at the front of the array are omitted from the final result" -body {
    flatten {"" "" 3}
} -returnCodes ok -result {3}

skip flatten-array-9
test flatten-array-9 "consecutive null values in the middle of the array are omitted from the final result" -body {
    flatten {1 "" "" 4}
} -returnCodes ok -result {1 4}

skip flatten-array-10
test flatten-array-10 "6 level nested array with null values" -body {
    flatten {0 2 {{2 3} 8 100 "" ""} -2}
} -returnCodes ok -result {0 2 2 3 8 100 -2}

skip flatten-array-11
test flatten-array-11 "all values in nested array are null" -body {
    flatten {"" "" "" "" {{"" ""} ""} ""}
} -returnCodes ok -result {}

skip flatten-array-12
test flatten-array-12 "Tcl can't differentiate between a list and a string" -body {
    flatten {foo bar {baz "a string with spaces"}}
} -returnCodes ok -result {foo bar baz a string with spaces}

cleanupTests
