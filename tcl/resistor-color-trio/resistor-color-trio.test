#!/usr/bin/env tclsh
# generated: 2026-07-23T13:38:59Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "resistor-color-trio.tcl"


test resistor-color-trio-1 "Orange and orange and black" -body {
    resistorColor::label orange orange black
} -returnCodes ok -result "33 ohms"

skip resistor-color-trio-2
test resistor-color-trio-2 "Blue and grey and brown" -body {
    resistorColor::label blue grey brown
} -returnCodes ok -result "680 ohms"

skip resistor-color-trio-3
test resistor-color-trio-3 "Red and black and red" -body {
    resistorColor::label red black red
} -returnCodes ok -result "2 kiloohms"

skip resistor-color-trio-4
test resistor-color-trio-4 "Green and brown and orange" -body {
    resistorColor::label green brown orange
} -returnCodes ok -result "51 kiloohms"

skip resistor-color-trio-5
test resistor-color-trio-5 "Yellow and violet and yellow" -body {
    resistorColor::label yellow violet yellow
} -returnCodes ok -result "470 kiloohms"

skip resistor-color-trio-6
test resistor-color-trio-6 "Blue and violet and blue" -body {
    resistorColor::label blue violet blue
} -returnCodes ok -result "67 megaohms"

skip resistor-color-trio-7
test resistor-color-trio-7 "Minimum possible value" -body {
    resistorColor::label black black black
} -returnCodes ok -result "0 ohms"

skip resistor-color-trio-8
test resistor-color-trio-8 "Maximum possible value" -body {
    resistorColor::label white white white
} -returnCodes ok -result "99 gigaohms"

skip resistor-color-trio-9
test resistor-color-trio-9 "First two colors make an invalid octal number" -body {
    resistorColor::label black grey black
} -returnCodes ok -result "8 ohms"

skip resistor-color-trio-10
test resistor-color-trio-10 "Ignore extra colors" -body {
    resistorColor::label blue green yellow orange
} -returnCodes ok -result "650 kiloohms"

# The canonical data for this exercise does not specify which
# is the expected result for this kind of scenario.
skip resistor-color-trio-extra
test resistor-color-trio-extra "value between 1000 and 10,000" -body {
    resistorColor::label brown red red
} -returnCodes ok -match inList -result {"1200 ohms" "1.2 kiloohms"}

cleanupTests
