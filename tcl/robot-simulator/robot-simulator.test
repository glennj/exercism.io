#!/usr/bin/env tclsh
# generated: 2026-07-23T15:56:59Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "robot-simulator.tcl"


test robot-simulator-1 "default position is (0,0) facing north" -body {
    set robot [Robot new]
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction north}

skip robot-simulator-2
test robot-simulator-2 "position (1,2) default direction" -body {
    set robot [Robot new {x 1 y 2}]
    $robot position
} -returnCodes ok -match dictionary -result {x 1 y 2 direction north}

skip robot-simulator-3
test robot-simulator-3 "Create robot: at origin facing north" -body {
    set robot [Robot new {x 0 y 0 direction north}]
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction north}

skip robot-simulator-4
test robot-simulator-4 "Create robot: at negative position facing south" -body {
    set robot [Robot new {x -1 y -1 direction south}]
    $robot position
} -returnCodes ok -match dictionary -result {x -1 y -1 direction south}

skip robot-simulator-5
test robot-simulator-5 "Rotating clockwise: changes north to east" -body {
    set robot [Robot new {x 0 y 0 direction north}]
    $robot move "R"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction east}

skip robot-simulator-6
test robot-simulator-6 "Rotating clockwise: changes east to south" -body {
    set robot [Robot new {x 0 y 0 direction east}]
    $robot move "R"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction south}

skip robot-simulator-7
test robot-simulator-7 "Rotating clockwise: changes south to west" -body {
    set robot [Robot new {x 0 y 0 direction south}]
    $robot move "R"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction west}

skip robot-simulator-8
test robot-simulator-8 "Rotating clockwise: changes west to north" -body {
    set robot [Robot new {x 0 y 0 direction west}]
    $robot move "R"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction north}

skip robot-simulator-9
test robot-simulator-9 "Rotating counter-clockwise: changes north to west" -body {
    set robot [Robot new {x 0 y 0 direction north}]
    $robot move "L"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction west}

skip robot-simulator-10
test robot-simulator-10 "Rotating counter-clockwise: changes west to south" -body {
    set robot [Robot new {x 0 y 0 direction west}]
    $robot move "L"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction south}

skip robot-simulator-11
test robot-simulator-11 "Rotating counter-clockwise: changes south to east" -body {
    set robot [Robot new {x 0 y 0 direction south}]
    $robot move "L"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction east}

skip robot-simulator-12
test robot-simulator-12 "Rotating counter-clockwise: changes east to north" -body {
    set robot [Robot new {x 0 y 0 direction east}]
    $robot move "L"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 0 direction north}

skip robot-simulator-13
test robot-simulator-13 "Moving forward one: facing north increments Y" -body {
    set robot [Robot new {x 0 y 0 direction north}]
    $robot move "A"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y 1 direction north}

skip robot-simulator-14
test robot-simulator-14 "Moving forward one: facing south decrements Y" -body {
    set robot [Robot new {x 0 y 0 direction south}]
    $robot move "A"
    $robot position
} -returnCodes ok -match dictionary -result {x 0 y -1 direction south}

skip robot-simulator-15
test robot-simulator-15 "Moving forward one: facing east increments X" -body {
    set robot [Robot new {x 0 y 0 direction east}]
    $robot move "A"
    $robot position
} -returnCodes ok -match dictionary -result {x 1 y 0 direction east}

skip robot-simulator-16
test robot-simulator-16 "Moving forward one: facing west decrements X" -body {
    set robot [Robot new {x 0 y 0 direction west}]
    $robot move "A"
    $robot position
} -returnCodes ok -match dictionary -result {x -1 y 0 direction west}

skip robot-simulator-17
test robot-simulator-17 "Follow series of instructions: moving east and north from README" -body {
    set robot [Robot new {x 7 y 3 direction north}]
    $robot move "RAALAL"
    $robot position
} -returnCodes ok -match dictionary -result {x 9 y 4 direction west}

skip robot-simulator-18
test robot-simulator-18 "Follow series of instructions: moving west and north" -body {
    set robot [Robot new {x 0 y 0 direction north}]
    $robot move "LAAARALA"
    $robot position
} -returnCodes ok -match dictionary -result {x -4 y 1 direction west}

skip robot-simulator-19
test robot-simulator-19 "Follow series of instructions: moving west and south" -body {
    set robot [Robot new {x 2 y -7 direction east}]
    $robot move "RRAAAAALA"
    $robot position
} -returnCodes ok -match dictionary -result {x -3 y -8 direction south}

skip robot-simulator-20
test robot-simulator-20 "Follow series of instructions: moving east and north" -body {
    set robot [Robot new {x 8 y 4 direction south}]
    $robot move "LAAARRRALLLL"
    $robot position
} -returnCodes ok -match dictionary -result {x 11 y 5 direction north}

skip robot-simulator-21
test robot-simulator-21 "invalid instruction" -body {
    set robot [Robot new]
    $robot move "RALB"
    $robot position
} -returnCodes error -result "invalid instruction: B"

cleanupTests
