#!/usr/bin/env tclsh
# generated: 2026-07-24T04:00:53Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "spiral-matrix.tcl"


test spiral-matrix-1 "empty spiral" -body {
    spiralMatrix 0
} -returnCodes ok -match listOfLists -result {}

skip spiral-matrix-2
test spiral-matrix-2 "trivial spiral" -body {
    spiralMatrix 1
} -returnCodes ok -match listOfLists -result {
    {1}
}

skip spiral-matrix-3
test spiral-matrix-3 "spiral of size 2" -body {
    spiralMatrix 2
} -returnCodes ok -match listOfLists -result {
    {1 2}
    {4 3}
}

skip spiral-matrix-4
test spiral-matrix-4 "spiral of size 3" -body {
    spiralMatrix 3
} -returnCodes ok -match listOfLists -result {
    {1 2 3}
    {8 9 4}
    {7 6 5}
}

skip spiral-matrix-5
test spiral-matrix-5 "spiral of size 4" -body {
    spiralMatrix 4
} -returnCodes ok -match listOfLists -result {
    {1 2 3 4}
    {12 13 14 5}
    {11 16 15 6}
    {10 9 8 7}
}

skip spiral-matrix-6
test spiral-matrix-6 "spiral of size 5" -body {
    spiralMatrix 5
} -returnCodes ok -match listOfLists -result {
    {1 2 3 4 5}
    {16 17 18 19 6}
    {15 24 25 20 7}
    {14 23 22 21 8}
    {13 12 11 10 9}
}


cleanupTests
