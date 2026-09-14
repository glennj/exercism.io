#!/usr/bin/env tclsh
# generated: 2026-07-18T18:51:02Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "nth-prime.tcl"


test nth-prime-1 "first prime" -body {
    nthPrime 1
} -returnCodes ok -result 2

skip nth-prime-2
test nth-prime-2 "second prime" -body {
    nthPrime 2
} -returnCodes ok -result 3

skip nth-prime-3
test nth-prime-3 "sixth prime" -body {
    nthPrime 6
} -returnCodes ok -result 13

skip nth-prime-4
test nth-prime-4 "big prime" -body {
    nthPrime 10001
} -returnCodes ok -result 104743

skip nth-prime-5
test nth-prime-5 "there is no zeroth prime" -body {
    nthPrime 0
} -returnCodes error -result "there is no zeroth prime"


cleanupTests
