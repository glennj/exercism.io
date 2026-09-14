#!/usr/bin/env tclsh
# generated: 2026-07-19T15:16:44Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "palindrome-products.tcl"


test palindrome-products-1 "find the smallest palindrome from single digit factors" -body {
    lassign [palindromeProducts smallest 1 9] value factors
    set expected 1
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{1 1}}

skip palindrome-products-2
test palindrome-products-2 "find the largest palindrome from single digit factors" -body {
    lassign [palindromeProducts largest 1 9] value factors
    set expected 9
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{1 9} {3 3}}

skip palindrome-products-3
test palindrome-products-3 "find the smallest palindrome from double digit factors" -body {
    lassign [palindromeProducts smallest 10 99] value factors
    set expected 121
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{11 11}}

skip palindrome-products-4
test palindrome-products-4 "find the largest palindrome from double digit factors" -body {
    lassign [palindromeProducts largest 10 99] value factors
    set expected 9009
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{91 99}}

skip palindrome-products-5
test palindrome-products-5 "find the smallest palindrome from triple digit factors" -body {
    lassign [palindromeProducts smallest 100 999] value factors
    set expected 10201
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{101 101}}

skip palindrome-products-6
test palindrome-products-6 "find the largest palindrome from triple digit factors" -body {
    lassign [palindromeProducts largest 100 999] value factors
    set expected 906609
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{913 993}}

skip palindrome-products-7
test palindrome-products-7 "find the smallest palindrome from four digit factors" -body {
    lassign [palindromeProducts smallest 1000 9999] value factors
    set expected 1002001
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{1001 1001}}

skip palindrome-products-8
test palindrome-products-8 "find the largest palindrome from four digit factors" -body {
    lassign [palindromeProducts largest 1000 9999] value factors
    set expected 99000099
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{9901 9999}}

skip palindrome-products-9
test palindrome-products-9 "empty result for smallest if no palindrome in the range" -body {
    lassign [palindromeProducts smallest 1002 1003] value factors
    set expected -1
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {}

skip palindrome-products-10
test palindrome-products-10 "empty result for largest if no palindrome in the range" -body {
    lassign [palindromeProducts largest 15 15] value factors
    set expected -1
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {}

skip palindrome-products-11
test palindrome-products-11 "error result for smallest if min is more than max" -body {
    lassign [palindromeProducts smallest 10000 1] value factors
} -returnCodes error -result "min must be <= max"

skip palindrome-products-12
test palindrome-products-12 "error result for largest if min is more than max" -body {
    lassign [palindromeProducts largest 2 1] value factors
} -returnCodes error -result "min must be <= max"

skip palindrome-products-13
test palindrome-products-13 "smallest product does not use the smallest factor" -body {
    lassign [palindromeProducts smallest 3215 4000] value factors
    set expected 10988901
    if {$value != $expected} {
        error "incorrect value: actual $value; expecting $expected"
    }
    set factors
} -returnCodes ok -match listOfLists -result {{3297 3333}}


cleanupTests
