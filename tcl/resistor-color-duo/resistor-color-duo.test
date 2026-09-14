#!/usr/bin/env tclsh
# generated: 2026-07-23T13:33:42Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "resistor-color-duo.tcl"


test resistor-color-duo-1 "Brown and black" -body {
    resistorColor::value brown black
} -returnCodes ok -result 10

skip resistor-color-duo-2
test resistor-color-duo-2 "Blue and grey" -body {
    resistorColor::value blue grey
} -returnCodes ok -result 68

skip resistor-color-duo-3
test resistor-color-duo-3 "Yellow and violet" -body {
    resistorColor::value yellow violet
} -returnCodes ok -result 47

skip resistor-color-duo-4
test resistor-color-duo-4 "White and red" -body {
    resistorColor::value white red
} -returnCodes ok -result 92

skip resistor-color-duo-5
test resistor-color-duo-5 "Orange and orange" -body {
    resistorColor::value orange orange
} -returnCodes ok -result 33

skip resistor-color-duo-6
test resistor-color-duo-6 "Ignore additional colors" -body {
    resistorColor::value green brown orange
} -returnCodes ok -result 51

skip resistor-color-duo-7
test resistor-color-duo-7 "Black and brown, one-digit" -body {
    resistorColor::value black brown
} -returnCodes ok -result 1

skip resistor-color-duo-err1
test resistor-color-duo-err1 "Invalid first color" -body {
    resistorColor::value foo black
} -returnCodes error -match glob -result {Invalid color*}

skip resistor-color-duo-err2
test resistor-color-duo-err2 "Invalid second color" -body {
    resistorColor::value black bar
} -returnCodes error -match glob -result {Invalid color*}

cleanupTests
