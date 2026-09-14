#!/usr/bin/env tclsh
# generated: 2026-07-16T21:52:57Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "armstrong-numbers.tcl"


test armstrong-numbers-1 "Zero is an Armstrong number" -body {
    isArmstrongNumber 0
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-2
test armstrong-numbers-2 "Single-digit numbers are Armstrong numbers" -body {
    isArmstrongNumber 5
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-3
test armstrong-numbers-3 "There are no two-digit Armstrong numbers" -body {
    isArmstrongNumber 10
} -returnCodes ok -match boolean -result false

skip armstrong-numbers-4
test armstrong-numbers-4 "Three-digit number that is an Armstrong number" -body {
    isArmstrongNumber 153
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-5
test armstrong-numbers-5 "Three-digit number that is not an Armstrong number" -body {
    isArmstrongNumber 100
} -returnCodes ok -match boolean -result false

skip armstrong-numbers-6
test armstrong-numbers-6 "Four-digit number that is an Armstrong number" -body {
    isArmstrongNumber 9474
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-7
test armstrong-numbers-7 "Four-digit number that is not an Armstrong number" -body {
    isArmstrongNumber 9475
} -returnCodes ok -match boolean -result false

skip armstrong-numbers-8
test armstrong-numbers-8 "Seven-digit number that is an Armstrong number" -body {
    isArmstrongNumber 9926315
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-9
test armstrong-numbers-9 "Seven-digit number that is not an Armstrong number" -body {
    isArmstrongNumber 9926314
} -returnCodes ok -match boolean -result false

skip armstrong-numbers-10
test armstrong-numbers-10 "Armstrong number containing seven zeroes" -body {
    isArmstrongNumber 186709961001538790100634132976990
} -returnCodes ok -match boolean -result true

skip armstrong-numbers-11
test armstrong-numbers-11 "The largest and last Armstrong number" -body {
    isArmstrongNumber 115132219018763992565095597973971522401
} -returnCodes ok -match boolean -result true


cleanupTests
