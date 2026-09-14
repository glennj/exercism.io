#!/usr/bin/env tclsh
# generated: 2026-07-17T13:34:44Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "collatz-conjecture.tcl"


test collatz-conjecture-1 "zero steps for one" -body {
    steps 1
} -returnCodes ok -result 0

skip collatz-conjecture-2
test collatz-conjecture-2 "divide if even" -body {
    steps 16
} -returnCodes ok -result 4

skip collatz-conjecture-3
test collatz-conjecture-3 "even and odd steps" -body {
    steps 12
} -returnCodes ok -result 9

skip collatz-conjecture-4
test collatz-conjecture-4 "large number of even and odd steps" -body {
    steps 1000000
} -returnCodes ok -result 152

skip collatz-conjecture-5
test collatz-conjecture-5 "zero is an error" -body {
    steps 0
} -returnCodes error -result "Only positive integers are allowed"

skip collatz-conjecture-6
test collatz-conjecture-6 "negative value is an error" -body {
    steps -15
} -returnCodes error -result "Only positive integers are allowed"


cleanupTests
