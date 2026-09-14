#!/usr/bin/env tclsh
# generated: 2026-07-17T16:05:10Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "book-store.tcl"


# results expressed in units of cents.

test book-store-1 "Only a single book" -body {
    basketCost {1}
} -returnCodes ok -result 800

skip book-store-2
test book-store-2 "Two of the same book" -body {
    basketCost {2 2}
} -returnCodes ok -result 1600

skip book-store-3
test book-store-3 "Empty basket" -body {
    basketCost {}
} -returnCodes ok -result 0

skip book-store-4
test book-store-4 "Two different books" -body {
    basketCost {1 2}
} -returnCodes ok -result 1520

skip book-store-5
test book-store-5 "Three different books" -body {
    basketCost {1 2 3}
} -returnCodes ok -result 2160

skip book-store-6
test book-store-6 "Four different books" -body {
    basketCost {1 2 3 4}
} -returnCodes ok -result 2560

skip book-store-7
test book-store-7 "Five different books" -body {
    basketCost {1 2 3 4 5}
} -returnCodes ok -result 3000

skip book-store-8
test book-store-8 "Two groups of four is cheaper than group of five plus group of three" -body {
    basketCost {1 1 2 2 3 3 4 5}
} -returnCodes ok -result 5120

skip book-store-9
test book-store-9 "Two groups of four is cheaper than groups of five and three" -body {
    basketCost {1 1 2 3 4 4 5 5}
} -returnCodes ok -result 5120

skip book-store-10
test book-store-10 "Group of four plus group of two is cheaper than two groups of three" -body {
    basketCost {1 1 2 2 3 4}
} -returnCodes ok -result 4080

skip book-store-11
test book-store-11 "Two each of first four books and one copy each of rest" -body {
    basketCost {1 1 2 2 3 3 4 4 5}
} -returnCodes ok -result 5560

skip book-store-12
test book-store-12 "Two copies of each book" -body {
    basketCost {1 1 2 2 3 3 4 4 5 5}
} -returnCodes ok -result 6000

skip book-store-13
test book-store-13 "Three copies of first book and two each of remaining" -body {
    basketCost {1 1 2 2 3 3 4 4 5 5 1}
} -returnCodes ok -result 6800

skip book-store-14
test book-store-14 "Three each of first two books and two each of remaining books" -body {
    basketCost {1 1 2 2 3 3 4 4 5 5 1 2}
} -returnCodes ok -result 7520

skip book-store-15
test book-store-15 "Four groups of four are cheaper than two groups each of five and three" -body {
    basketCost {1 1 2 2 3 3 4 5 1 1 2 2 3 3 4 5}
} -returnCodes ok -result 10240

# this may take a long time: change 0 to 1 to run this test case.
if {0} {
skip book-store-16
test book-store-16 "Check that groups of four are created properly even when there are more groups of three than groups of five" -body {
    basketCost {1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 5 5}
} -returnCodes ok -result 14560
}

skip book-store-17
test book-store-17 "One group of one and four is cheaper than one group of two and three" -body {
    basketCost {1 1 2 3 4}
} -returnCodes ok -result 3360

skip book-store-18
test book-store-18 "One group of one and two plus three groups of four is cheaper than one group of each size" -body {
    basketCost {1 2 2 3 3 3 4 4 4 4 5 5 5 5 5}
} -returnCodes ok -result 10000


cleanupTests
