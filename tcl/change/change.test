#!/usr/bin/env tclsh
# generated: 2026-07-17T10:53:17Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "change.tcl"


test change-1 "change for 1 cent" -body {
    findMinimumCoins 1 {1 5 10 25}
} -returnCodes ok -result {1}

skip change-2
test change-2 "single coin change" -body {
    findMinimumCoins 25 {1 5 10 25 100}
} -returnCodes ok -result {25}

skip change-3
test change-3 "multiple coin change" -body {
    findMinimumCoins 15 {1 5 10 25 100}
} -returnCodes ok -result {5 10}

skip change-4
test change-4 "change with Lilliputian Coins" -body {
    findMinimumCoins 23 {1 4 15 20 50}
} -returnCodes ok -result {4 4 15}

skip change-5
test change-5 "change with Lower Elbonia Coins" -body {
    findMinimumCoins 63 {1 5 10 21 25}
} -returnCodes ok -result {21 21 21}

skip change-6
test change-6 "large target values" -body {
    findMinimumCoins 999 {1 2 5 10 20 50 100}
} -returnCodes ok -result {2 2 5 20 20 50 100 100 100 100 100 100 100 100 100}

skip change-7
test change-7 "possible change without unit coins available" -body {
    findMinimumCoins 21 {2 5 10 20 50}
} -returnCodes ok -result {2 2 2 5 10}

skip change-8
test change-8 "another possible change without unit coins available" -body {
    findMinimumCoins 27 {4 5}
} -returnCodes ok -result {4 4 4 5 5 5}

skip change-9
test change-9 "a greedy approach is not optimal" -body {
    findMinimumCoins 20 {1 10 11}
} -returnCodes ok -result {10 10}

skip change-10
test change-10 "no coins make 0 change" -body {
    findMinimumCoins 0 {1 5 10 21 25}
} -returnCodes ok -result {}

skip change-11
test change-11 "error testing for change smaller than the smallest of coins" -body {
    findMinimumCoins 3 {5 10}
} -returnCodes error -result "can't make target with given coins"

skip change-12
test change-12 "error if no combination can add up to target" -body {
    findMinimumCoins 94 {5 10}
} -returnCodes error -result "can't make target with given coins"

skip change-13
test change-13 "cannot find negative change values" -body {
    findMinimumCoins -5 {1 2 5}
} -returnCodes error -result "target can't be negative"


cleanupTests
