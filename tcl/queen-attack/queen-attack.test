#!/usr/bin/env tclsh
# generated: 2026-07-23T11:55:58Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "queen-attack.tcl"


test queen-attack-1 "Test creation of Queens with valid and invalid positions: queen with a valid position" -body {
    set queen [Queen new 2 2]
    info object isa typeof $queen Queen
} -returnCodes ok -match boolean -result true

skip queen-attack-2
test queen-attack-2 "Test creation of Queens with valid and invalid positions: queen must have positive row" -body {
    set queen [Queen new -2 2]
} -returnCodes error -result "row not positive"

skip queen-attack-3
test queen-attack-3 "Test creation of Queens with valid and invalid positions: queen must have row on board" -body {
    set queen [Queen new 8 4]
} -returnCodes error -result "row not on board"

skip queen-attack-4
test queen-attack-4 "Test creation of Queens with valid and invalid positions: queen must have positive column" -body {
    set queen [Queen new 2 -2]
} -returnCodes error -result "column not positive"

skip queen-attack-5
test queen-attack-5 "Test creation of Queens with valid and invalid positions: queen must have column on board" -body {
    set queen [Queen new 4 8]
} -returnCodes error -result "column not on board"

skip queen-attack-6
test queen-attack-6 "Test the ability of one queen to attack another: cannot attack" -body {
    set q1 [Queen new 6 6]
    set q2 [Queen new 2 4]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result false
skip queen-attack-7
test queen-attack-7 "Test the ability of one queen to attack another: can attack on same row" -body {
    set q1 [Queen new 2 6]
    set q2 [Queen new 2 4]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-8
test queen-attack-8 "Test the ability of one queen to attack another: can attack on same column" -body {
    set q1 [Queen new 2 5]
    set q2 [Queen new 4 5]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-9
test queen-attack-9 "Test the ability of one queen to attack another: can attack on first diagonal" -body {
    set q1 [Queen new 0 4]
    set q2 [Queen new 2 2]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-10
test queen-attack-10 "Test the ability of one queen to attack another: can attack on second diagonal" -body {
    set q1 [Queen new 3 1]
    set q2 [Queen new 2 2]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-11
test queen-attack-11 "Test the ability of one queen to attack another: can attack on third diagonal" -body {
    set q1 [Queen new 1 1]
    set q2 [Queen new 2 2]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-12
test queen-attack-12 "Test the ability of one queen to attack another: can attack on fourth diagonal" -body {
    set q1 [Queen new 0 6]
    set q2 [Queen new 1 7]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result true
skip queen-attack-13
test queen-attack-13 "Test the ability of one queen to attack another: cannot attack if falling diagonals are only the same when reflected across the longest falling diagonal" -body {
    set q1 [Queen new 2 5]
    set q2 [Queen new 4 1]
    $q1 canAttack $q2
} -returnCodes ok -match boolean -result false

cleanupTests
