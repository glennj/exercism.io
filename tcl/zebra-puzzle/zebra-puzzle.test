#!/usr/bin/env tclsh
# generated: 2026-07-24T19:15:41Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "zebra-puzzle.tcl"


test zebra-puzzle-1 "resident who drinks water" -body {
    set zp [ZebraPuzzle new]
    $zp drinksWater
} -returnCodes ok -result "Norwegian"

skip zebra-puzzle-2
test zebra-puzzle-2 "resident who owns zebra" -body {
    set zp [ZebraPuzzle new]
    $zp ownsZebra
} -returnCodes ok -result "Japanese"


cleanupTests
