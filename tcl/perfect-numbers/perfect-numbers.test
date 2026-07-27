#!/usr/bin/env tclsh
# generated: 2026-07-19T14:48:00Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "perfect-numbers.tcl"


test perfect-numbers-1 "Perfect numbers: Smallest perfect number is classified correctly" -body {
    classify 6
} -returnCodes ok -result perfect

skip perfect-numbers-2
test perfect-numbers-2 "Perfect numbers: Medium perfect number is classified correctly" -body {
    classify 28
} -returnCodes ok -result perfect

skip perfect-numbers-3
test perfect-numbers-3 "Perfect numbers: Large perfect number is classified correctly" -body {
    classify 33550336
} -returnCodes ok -result perfect

skip perfect-numbers-4
test perfect-numbers-4 "Abundant numbers: Smallest abundant number is classified correctly" -body {
    classify 12
} -returnCodes ok -result abundant

skip perfect-numbers-5
test perfect-numbers-5 "Abundant numbers: Medium abundant number is classified correctly" -body {
    classify 30
} -returnCodes ok -result abundant

skip perfect-numbers-6
test perfect-numbers-6 "Abundant numbers: Large abundant number is classified correctly" -body {
    classify 33550335
} -returnCodes ok -result abundant

skip perfect-numbers-7
test perfect-numbers-7 "Abundant numbers: Perfect square abundant number is classified correctly" -body {
    classify 196
} -returnCodes ok -result abundant

skip perfect-numbers-8
test perfect-numbers-8 "Deficient numbers: Smallest prime deficient number is classified correctly" -body {
    classify 2
} -returnCodes ok -result deficient

skip perfect-numbers-9
test perfect-numbers-9 "Deficient numbers: Smallest non-prime deficient number is classified correctly" -body {
    classify 4
} -returnCodes ok -result deficient

skip perfect-numbers-10
test perfect-numbers-10 "Deficient numbers: Medium deficient number is classified correctly" -body {
    classify 32
} -returnCodes ok -result deficient

skip perfect-numbers-11
test perfect-numbers-11 "Deficient numbers: Large deficient number is classified correctly" -body {
    classify 33550337
} -returnCodes ok -result deficient

skip perfect-numbers-12
test perfect-numbers-12 "Deficient numbers: Edge case (no factors other than itself) is classified correctly" -body {
    classify 1
} -returnCodes ok -result deficient

skip perfect-numbers-13
test perfect-numbers-13 "Invalid inputs: Zero is rejected (as it is not a positive integer)" -body {
    classify 0
} -returnCodes error -result "Classification is only possible for positive integers."

skip perfect-numbers-14
test perfect-numbers-14 "Invalid inputs: Negative integer is rejected (as it is not a positive integer)" -body {
    classify -1
} -returnCodes error -result "Classification is only possible for positive integers."


cleanupTests
