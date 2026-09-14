#!/usr/bin/env tclsh
# generated: 2026-07-24T17:43:32Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "square-root.tcl"


test square-root-1 "root of 1" -body {
    squareRoot 1
} -returnCodes ok -result 1

skip square-root-2
test square-root-2 "root of 4" -body {
    squareRoot 4
} -returnCodes ok -result 2

skip square-root-3
test square-root-3 "root of 25" -body {
    squareRoot 25
} -returnCodes ok -result 5

skip square-root-4
test square-root-4 "root of 81" -body {
    squareRoot 81
} -returnCodes ok -result 9

skip square-root-5
test square-root-5 "root of 196" -body {
    squareRoot 196
} -returnCodes ok -result 14

skip square-root-6
test square-root-6 "root of 65025" -body {
    squareRoot 65025
} -returnCodes ok -result 255


cleanupTests
