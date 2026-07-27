#!/usr/bin/env tclsh
# generated: 2026-07-24T02:02:47Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "series.tcl"


test series-1 "slices of one from one" -body {
    slices "1" 1
} -returnCodes ok -match orderedLists -result {"1"}

skip series-2
test series-2 "slices of one from two" -body {
    slices "12" 1
} -returnCodes ok -match orderedLists -result {"1" "2"}

skip series-3
test series-3 "slices of two" -body {
    slices "35" 2
} -returnCodes ok -match orderedLists -result {"35"}

skip series-4
test series-4 "slices of two overlap" -body {
    slices "9142" 2
} -returnCodes ok -match orderedLists -result {"91" "14" "42"}

skip series-5
test series-5 "slices can include duplicates" -body {
    slices "777777" 3
} -returnCodes ok -match orderedLists -result {"777" "777" "777" "777"}

skip series-6
test series-6 "slices of a long series" -body {
    slices "918493904243" 5
} -returnCodes ok -match orderedLists -result {"91849" "18493" "84939" "49390" "93904" "39042" "90424" "04243"}

skip series-7
test series-7 "slice length is too large" -body {
    slices "12345" 6
} -returnCodes error -result "slice length cannot be greater than series length"

skip series-8
test series-8 "slice length is way too large" -body {
    slices "12345" 42
} -returnCodes error -result "slice length cannot be greater than series length"

skip series-9
test series-9 "slice length cannot be zero" -body {
    slices "12345" 0
} -returnCodes error -result "slice length cannot be zero"

skip series-10
test series-10 "slice length cannot be negative" -body {
    slices "123" -1
} -returnCodes error -result "slice length cannot be negative"

skip series-11
test series-11 "empty series is invalid" -body {
    slices "" 1
} -returnCodes error -result "series cannot be empty"


cleanupTests
