#!/usr/bin/env tclsh
# generated: 2026-07-18T01:34:50Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "isbn-verifier.tcl"


test isbn-verifier-1 "valid isbn" -body {
    isValid "3-598-21508-8"
} -returnCodes ok -match boolean -result true

skip isbn-verifier-2
test isbn-verifier-2 "invalid isbn check digit" -body {
    isValid "3-598-21508-9"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-3
test isbn-verifier-3 "valid isbn with a check digit of 10" -body {
    isValid "3-598-21507-X"
} -returnCodes ok -match boolean -result true

skip isbn-verifier-4
test isbn-verifier-4 "check digit is a character other than X" -body {
    isValid "3-598-21507-A"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-5
test isbn-verifier-5 "invalid check digit in isbn is not treated as zero" -body {
    isValid "4-598-21507-B"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-6
test isbn-verifier-6 "invalid character in isbn is not treated as zero" -body {
    isValid "3-598-P1581-X"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-7
test isbn-verifier-7 "X is only valid as a check digit" -body {
    isValid "3-598-2X507-9"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-8
test isbn-verifier-8 "only one check digit is allowed" -body {
    isValid "3-598-21508-96"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-9
test isbn-verifier-9 "X is not substituted by the value 10" -body {
    isValid "3-598-2X507-5"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-10
test isbn-verifier-10 "valid isbn without separating dashes" -body {
    isValid "3598215088"
} -returnCodes ok -match boolean -result true

skip isbn-verifier-11
test isbn-verifier-11 "isbn without separating dashes and X as check digit" -body {
    isValid "359821507X"
} -returnCodes ok -match boolean -result true

skip isbn-verifier-12
test isbn-verifier-12 "isbn without check digit and dashes" -body {
    isValid "359821507"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-13
test isbn-verifier-13 "too long isbn and no dashes" -body {
    isValid "3598215078X"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-14
test isbn-verifier-14 "too short isbn" -body {
    isValid "00"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-15
test isbn-verifier-15 "isbn without check digit" -body {
    isValid "3-598-21507"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-16
test isbn-verifier-16 "check digit of X should not be used for 0" -body {
    isValid "3-598-21515-X"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-17
test isbn-verifier-17 "empty isbn" -body {
    isValid ""
} -returnCodes ok -match boolean -result false

skip isbn-verifier-18
test isbn-verifier-18 "input is 9 characters" -body {
    isValid "134456729"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-19
test isbn-verifier-19 "invalid characters are not ignored after checking length" -body {
    isValid "3132P34035"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-20
test isbn-verifier-20 "invalid characters are not ignored before checking length" -body {
    isValid "3598P215088"
} -returnCodes ok -match boolean -result false

skip isbn-verifier-21
test isbn-verifier-21 "input is too long but contains a valid isbn" -body {
    isValid "98245726788"
} -returnCodes ok -match boolean -result false


cleanupTests
