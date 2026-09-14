#!/usr/bin/env tclsh
# generated: 2026-07-23T13:28:27Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "resistor-color.tcl"


test resistor-color-1 "Color codes: Black" -body {
    resistorColor::colorCode black
} -returnCodes ok -result 0

skip resistor-color-2
test resistor-color-2 "Color codes: White" -body {
    resistorColor::colorCode white
} -returnCodes ok -result 9

skip resistor-color-3
test resistor-color-3 "Color codes: Orange" -body {
    resistorColor::colorCode orange
} -returnCodes ok -result 3

skip resistor-color-4
test resistor-color-4 "Colors" -body {
    resistorColor::colors
} -returnCodes ok -match orderedLists -result {black brown red orange yellow green blue violet grey white}

skip resistor-color-err-1
test resistor-color-err-1 "Unknown color" -body {
    resistorColor::colorCode beige
} -returnCodes error -result "Invalid color: beige"

skip resistor-color-err-2
test resistor-color-err-2 "test for lsearch with glob" -body {
    resistorColor::colorCode "bl*"
} -returnCodes error -result "Invalid color: bl*"

cleanupTests
