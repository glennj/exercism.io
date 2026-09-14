#!/usr/bin/env tclsh
# generated: 2026-07-17T18:59:47Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "difference-of-squares.tcl"


test difference-of-squares-1 "Square the sum of the numbers up to the given number: square of sum 1" -body {
    squareOfSum 1
} -returnCodes ok -result 1

skip difference-of-squares-2
test difference-of-squares-2 "Square the sum of the numbers up to the given number: square of sum 5" -body {
    squareOfSum 5
} -returnCodes ok -result 225

skip difference-of-squares-3
test difference-of-squares-3 "Square the sum of the numbers up to the given number: square of sum 100" -body {
    squareOfSum 100
} -returnCodes ok -result 25502500

skip difference-of-squares-4
test difference-of-squares-4 "Sum the squares of the numbers up to the given number: sum of squares 1" -body {
    sumOfSquares 1
} -returnCodes ok -result 1

skip difference-of-squares-5
test difference-of-squares-5 "Sum the squares of the numbers up to the given number: sum of squares 5" -body {
    sumOfSquares 5
} -returnCodes ok -result 55

skip difference-of-squares-6
test difference-of-squares-6 "Sum the squares of the numbers up to the given number: sum of squares 100" -body {
    sumOfSquares 100
} -returnCodes ok -result 338350

skip difference-of-squares-7
test difference-of-squares-7 "Subtract sum of squares from square of sums: difference of squares 1" -body {
    differenceOfSquares 1
} -returnCodes ok -result 0

skip difference-of-squares-8
test difference-of-squares-8 "Subtract sum of squares from square of sums: difference of squares 5" -body {
    differenceOfSquares 5
} -returnCodes ok -result 170

skip difference-of-squares-9
test difference-of-squares-9 "Subtract sum of squares from square of sums: difference of squares 100" -body {
    differenceOfSquares 100
} -returnCodes ok -result 25164150


cleanupTests
