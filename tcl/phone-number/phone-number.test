#!/usr/bin/env tclsh
# generated: 2026-07-19T14:50:03Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "phone-number.tcl"


test phone-number-1 "cleans the number" -body {
    clean "(223) 456-7890"
} -returnCodes ok -result 2234567890

skip phone-number-2
test phone-number-2 "cleans numbers with dots" -body {
    clean "223.456.7890"
} -returnCodes ok -result 2234567890

skip phone-number-3
test phone-number-3 "cleans numbers with multiple spaces" -body {
    clean "223 456   7890   "
} -returnCodes ok -result 2234567890

skip phone-number-4
test phone-number-4 "invalid when 9 digits" -body {
    clean "123456789"
} -returnCodes error -result "must not be fewer than 10 digits"

skip phone-number-5
test phone-number-5 "invalid when 11 digits does not start with a 1" -body {
    clean "22234567890"
} -returnCodes error -result "11 digits must start with 1"

skip phone-number-6
test phone-number-6 "valid when 11 digits and starting with 1" -body {
    clean "12234567890"
} -returnCodes ok -result 2234567890

skip phone-number-7
test phone-number-7 "valid when 11 digits and starting with 1 even with punctuation" -body {
    clean "+1 (223) 456-7890"
} -returnCodes ok -result 2234567890

skip phone-number-8
test phone-number-8 "invalid when more than 11 digits" -body {
    clean "321234567890"
} -returnCodes error -result "must not be greater than 11 digits"

skip phone-number-9
test phone-number-9 "invalid with letters" -body {
    clean "523-abc-7890"
} -returnCodes error -result "letters not permitted"

skip phone-number-10
test phone-number-10 "invalid with punctuations" -body {
    clean "523-@:!-7890"
} -returnCodes error -result "punctuations not permitted"

skip phone-number-11
test phone-number-11 "invalid if area code starts with 0" -body {
    clean "(023) 456-7890"
} -returnCodes error -result "area code cannot start with zero"

skip phone-number-12
test phone-number-12 "invalid if area code starts with 1" -body {
    clean "(123) 456-7890"
} -returnCodes error -result "area code cannot start with one"

skip phone-number-13
test phone-number-13 "invalid if exchange code starts with 0" -body {
    clean "(223) 056-7890"
} -returnCodes error -result "exchange code cannot start with zero"

skip phone-number-14
test phone-number-14 "invalid if exchange code starts with 1" -body {
    clean "(223) 156-7890"
} -returnCodes error -result "exchange code cannot start with one"

skip phone-number-15
test phone-number-15 "invalid if area code starts with 0 on valid 11-digit number" -body {
    clean "1 (023) 456-7890"
} -returnCodes error -result "area code cannot start with zero"

skip phone-number-16
test phone-number-16 "invalid if area code starts with 1 on valid 11-digit number" -body {
    clean "1 (123) 456-7890"
} -returnCodes error -result "area code cannot start with one"

skip phone-number-17
test phone-number-17 "invalid if exchange code starts with 0 on valid 11-digit number" -body {
    clean "1 (223) 056-7890"
} -returnCodes error -result "exchange code cannot start with zero"

skip phone-number-18
test phone-number-18 "invalid if exchange code starts with 1 on valid 11-digit number" -body {
    clean "1 (223) 156-7890"
} -returnCodes error -result "exchange code cannot start with one"


cleanupTests
