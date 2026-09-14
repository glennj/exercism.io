#!/usr/bin/env tclsh
# generated: 2026-07-24T19:09:25Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "wordy.tcl"


test wordy-1 "just a number" -body {
    answer "What is 5?"
} -returnCodes ok -result 5

skip wordy-2
test wordy-2 "just a zero" -body {
    answer "What is 0?"
} -returnCodes ok -result 0

skip wordy-3
test wordy-3 "just a negative number" -body {
    answer "What is -123?"
} -returnCodes ok -result -123

skip wordy-4
test wordy-4 "addition" -body {
    answer "What is 1 plus 1?"
} -returnCodes ok -result 2

skip wordy-5
test wordy-5 "addition with a left hand zero" -body {
    answer "What is 0 plus 2?"
} -returnCodes ok -result 2

skip wordy-6
test wordy-6 "addition with a right hand zero" -body {
    answer "What is 3 plus 0?"
} -returnCodes ok -result 3

skip wordy-7
test wordy-7 "more addition" -body {
    answer "What is 53 plus 2?"
} -returnCodes ok -result 55

skip wordy-8
test wordy-8 "addition with negative numbers" -body {
    answer "What is -1 plus -10?"
} -returnCodes ok -result -11

skip wordy-9
test wordy-9 "large addition" -body {
    answer "What is 123 plus 45678?"
} -returnCodes ok -result 45801

skip wordy-10
test wordy-10 "subtraction" -body {
    answer "What is 4 minus -12?"
} -returnCodes ok -result 16

skip wordy-11
test wordy-11 "multiplication" -body {
    answer "What is -3 multiplied by 25?"
} -returnCodes ok -result -75

skip wordy-12
test wordy-12 "division" -body {
    answer "What is 33 divided by -3?"
} -returnCodes ok -result -11

skip wordy-13
test wordy-13 "multiple additions" -body {
    answer "What is 1 plus 1 plus 1?"
} -returnCodes ok -result 3

skip wordy-14
test wordy-14 "addition and subtraction" -body {
    answer "What is 1 plus 5 minus -2?"
} -returnCodes ok -result 8

skip wordy-15
test wordy-15 "multiple subtraction" -body {
    answer "What is 20 minus 4 minus 13?"
} -returnCodes ok -result 3

skip wordy-16
test wordy-16 "subtraction then addition" -body {
    answer "What is 17 minus 6 plus 3?"
} -returnCodes ok -result 14

skip wordy-17
test wordy-17 "multiple multiplication" -body {
    answer "What is 2 multiplied by -2 multiplied by 3?"
} -returnCodes ok -result -12

skip wordy-18
test wordy-18 "addition and multiplication" -body {
    answer "What is -3 plus 7 multiplied by -2?"
} -returnCodes ok -result -8

skip wordy-19
test wordy-19 "multiple division" -body {
    answer "What is -12 divided by 2 divided by -3?"
} -returnCodes ok -result 2

skip wordy-20
test wordy-20 "unknown operation" -body {
    answer "What is 52 cubed?"
} -returnCodes error -result "unknown operation"

skip wordy-21
test wordy-21 "Non math question" -body {
    answer "Who is the President of the United States?"
} -returnCodes error -result "unknown operation"

skip wordy-22
test wordy-22 "reject problem missing an operand" -body {
    answer "What is 1 plus?"
} -returnCodes error -result "syntax error"

skip wordy-23
test wordy-23 "reject problem with no operands or operators" -body {
    answer "What is?"
} -returnCodes error -result "syntax error"

skip wordy-24
test wordy-24 "reject two operations in a row" -body {
    answer "What is 1 plus plus 2?"
} -returnCodes error -result "syntax error"

skip wordy-25
test wordy-25 "reject two numbers in a row" -body {
    answer "What is 1 plus 2 1?"
} -returnCodes error -result "syntax error"

skip wordy-26
test wordy-26 "reject postfix notation" -body {
    answer "What is 1 2 plus?"
} -returnCodes error -result "syntax error"

skip wordy-27
test wordy-27 "reject prefix notation" -body {
    answer "What is plus 1 2?"
} -returnCodes error -result "syntax error"


cleanupTests
