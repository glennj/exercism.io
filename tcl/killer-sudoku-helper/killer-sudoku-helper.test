#!/usr/bin/env tclsh
# generated: 2026-07-18T01:46:19Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "killer-sudoku-helper.tcl"


test killer-sudoku-helper-1 "Trivial 1-digit cages: 1" -body {
    combinations 1 1 {}
} -returnCodes ok -match listOfLists -result {1}

skip killer-sudoku-helper-2
test killer-sudoku-helper-2 "Trivial 1-digit cages: 2" -body {
    combinations 2 1 {}
} -returnCodes ok -match listOfLists -result {2}

skip killer-sudoku-helper-3
test killer-sudoku-helper-3 "Trivial 1-digit cages: 3" -body {
    combinations 3 1 {}
} -returnCodes ok -match listOfLists -result {3}

skip killer-sudoku-helper-4
test killer-sudoku-helper-4 "Trivial 1-digit cages: 4" -body {
    combinations 4 1 {}
} -returnCodes ok -match listOfLists -result {4}

skip killer-sudoku-helper-5
test killer-sudoku-helper-5 "Trivial 1-digit cages: 5" -body {
    combinations 5 1 {}
} -returnCodes ok -match listOfLists -result {5}

skip killer-sudoku-helper-6
test killer-sudoku-helper-6 "Trivial 1-digit cages: 6" -body {
    combinations 6 1 {}
} -returnCodes ok -match listOfLists -result {6}

skip killer-sudoku-helper-7
test killer-sudoku-helper-7 "Trivial 1-digit cages: 7" -body {
    combinations 7 1 {}
} -returnCodes ok -match listOfLists -result {7}

skip killer-sudoku-helper-8
test killer-sudoku-helper-8 "Trivial 1-digit cages: 8" -body {
    combinations 8 1 {}
} -returnCodes ok -match listOfLists -result {8}

skip killer-sudoku-helper-9
test killer-sudoku-helper-9 "Trivial 1-digit cages: 9" -body {
    combinations 9 1 {}
} -returnCodes ok -match listOfLists -result {9}

skip killer-sudoku-helper-10
test killer-sudoku-helper-10 "Cage with sum 45 contains all digits 1:9" -body {
    combinations 45 9 {}
} -returnCodes ok -match listOfLists -result {{1 2 3 4 5 6 7 8 9}}

skip killer-sudoku-helper-11
test killer-sudoku-helper-11 "Cage with only 1 possible combination" -body {
    combinations 7 3 {}
} -returnCodes ok -match listOfLists -result {{1 2 4}}

skip killer-sudoku-helper-12
test killer-sudoku-helper-12 "Cage with several combinations" -body {
    combinations 10 2 {}
} -returnCodes ok -match listOfLists -result {{1 9} {2 8} {3 7} {4 6}}

skip killer-sudoku-helper-13
test killer-sudoku-helper-13 "Cage with several combinations that is restricted" -body {
    combinations 10 2 {1 4}
} -returnCodes ok -match listOfLists -result {{2 8} {3 7}}


cleanupTests
