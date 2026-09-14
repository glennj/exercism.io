#!/usr/bin/env tclsh
# generated: 2026-07-24T19:22:58Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "pascals-triangle.tcl"


test pascals-triangle-1 "zero rows" -body {
    triangle 0
} -returnCodes ok -match orderedLists -result {}

skip pascals-triangle-2
test pascals-triangle-2 "single row" -body {
    triangle 1
} -returnCodes ok -match orderedLists -result {
    {1}
}

skip pascals-triangle-3
test pascals-triangle-3 "two rows" -body {
    triangle 2
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
}

skip pascals-triangle-4
test pascals-triangle-4 "three rows" -body {
    triangle 3
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
    {1 2 1}
}

skip pascals-triangle-5
test pascals-triangle-5 "four rows" -body {
    triangle 4
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
    {1 2 1}
    {1 3 3 1}
}

skip pascals-triangle-6
test pascals-triangle-6 "five rows" -body {
    triangle 5
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
    {1 2 1}
    {1 3 3 1}
    {1 4 6 4 1}
}

skip pascals-triangle-7
test pascals-triangle-7 "six rows" -body {
    triangle 6
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
    {1 2 1}
    {1 3 3 1}
    {1 4 6 4 1}
    {1 5 10 10 5 1}
}

skip pascals-triangle-8
test pascals-triangle-8 "ten rows" -body {
    triangle 10
} -returnCodes ok -match orderedLists -result {
    {1}
    {1 1}
    {1 2 1}
    {1 3 3 1}
    {1 4 6 4 1}
    {1 5 10 10 5 1}
    {1 6 15 20 15 6 1}
    {1 7 21 35 35 21 7 1}
    {1 8 28 56 70 56 28 8 1}
    {1 9 36 84 126 126 84 36 9 1}
}


cleanupTests
