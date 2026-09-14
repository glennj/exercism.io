#!/usr/bin/env tclsh
# generated: 2026-07-17T17:47:10Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "complex-numbers.tcl"


test complex-numbers-1 "Real part: Real part of a purely real number" -body {
    set c [ComplexNumber new 1 0]
    $c real
} -returnCodes ok -result 1

skip complex-numbers-2
test complex-numbers-2 "Real part: Real part of a purely imaginary number" -body {
    set c [ComplexNumber new 0 1]
    $c real
} -returnCodes ok -result 0

skip complex-numbers-3
test complex-numbers-3 "Real part: Real part of a number with real and imaginary part" -body {
    set c [ComplexNumber new 1 2]
    $c real
} -returnCodes ok -result 1

skip complex-numbers-4
test complex-numbers-4 "Imaginary part: Imaginary part of a purely real number" -body {
    set c [ComplexNumber new 1 0]
    $c imag
} -returnCodes ok -result 0

skip complex-numbers-5
test complex-numbers-5 "Imaginary part: Imaginary part of a purely imaginary number" -body {
    set c [ComplexNumber new 0 1]
    $c imag
} -returnCodes ok -result 1

skip complex-numbers-6
test complex-numbers-6 "Imaginary part: Imaginary part of a number with real and imaginary part" -body {
    set c [ComplexNumber new 1 2]
    $c imag
} -returnCodes ok -result 2

skip complex-numbers-7
test complex-numbers-7 "Imaginary unit" -body {
    set c1 [ComplexNumber new 0 1]
    set c2 [ComplexNumber new 0 1]
    [$c1 mul $c2] toList
} -returnCodes ok -match pairOfFloats -result {-1 0}

skip complex-numbers-8
test complex-numbers-8 "Arithmetic: Addition: Add purely real numbers" -body {
    set c1 [ComplexNumber new 1 0]
    set c2 [ComplexNumber new 2 0]
    [$c1 add $c2] toList
} -returnCodes ok -match pairOfFloats -result {3 0}

skip complex-numbers-9
test complex-numbers-9 "Arithmetic: Addition: Add purely imaginary numbers" -body {
    set c1 [ComplexNumber new 0 1]
    set c2 [ComplexNumber new 0 2]
    [$c1 add $c2] toList
} -returnCodes ok -match pairOfFloats -result {0 3}

skip complex-numbers-10
test complex-numbers-10 "Arithmetic: Addition: Add numbers with real and imaginary part" -body {
    set c1 [ComplexNumber new 1 2]
    set c2 [ComplexNumber new 3 4]
    [$c1 add $c2] toList
} -returnCodes ok -match pairOfFloats -result {4 6}

skip complex-numbers-11
test complex-numbers-11 "Arithmetic: Subtraction: Subtract purely real numbers" -body {
    set c1 [ComplexNumber new 1 0]
    set c2 [ComplexNumber new 2 0]
    [$c1 sub $c2] toList
} -returnCodes ok -match pairOfFloats -result {-1 0}

skip complex-numbers-12
test complex-numbers-12 "Arithmetic: Subtraction: Subtract purely imaginary numbers" -body {
    set c1 [ComplexNumber new 0 1]
    set c2 [ComplexNumber new 0 2]
    [$c1 sub $c2] toList
} -returnCodes ok -match pairOfFloats -result {0 -1}

skip complex-numbers-13
test complex-numbers-13 "Arithmetic: Subtraction: Subtract numbers with real and imaginary part" -body {
    set c1 [ComplexNumber new 1 2]
    set c2 [ComplexNumber new 3 4]
    [$c1 sub $c2] toList
} -returnCodes ok -match pairOfFloats -result {-2 -2}

skip complex-numbers-14
test complex-numbers-14 "Arithmetic: Multiplication: Multiply purely real numbers" -body {
    set c1 [ComplexNumber new 1 0]
    set c2 [ComplexNumber new 2 0]
    [$c1 mul $c2] toList
} -returnCodes ok -match pairOfFloats -result {2 0}

skip complex-numbers-15
test complex-numbers-15 "Arithmetic: Multiplication: Multiply purely imaginary numbers" -body {
    set c1 [ComplexNumber new 0 1]
    set c2 [ComplexNumber new 0 2]
    [$c1 mul $c2] toList
} -returnCodes ok -match pairOfFloats -result {-2 0}

skip complex-numbers-16
test complex-numbers-16 "Arithmetic: Multiplication: Multiply numbers with real and imaginary part" -body {
    set c1 [ComplexNumber new 1 2]
    set c2 [ComplexNumber new 3 4]
    [$c1 mul $c2] toList
} -returnCodes ok -match pairOfFloats -result {-5 10}

skip complex-numbers-17
test complex-numbers-17 "Arithmetic: Division: Divide purely real numbers" -body {
    set c1 [ComplexNumber new 1 0]
    set c2 [ComplexNumber new 2 0]
    [$c1 div $c2] toList
} -returnCodes ok -match pairOfFloats -result {0.5 0}

skip complex-numbers-18
test complex-numbers-18 "Arithmetic: Division: Divide purely imaginary numbers" -body {
    set c1 [ComplexNumber new 0 1]
    set c2 [ComplexNumber new 0 2]
    [$c1 div $c2] toList
} -returnCodes ok -match pairOfFloats -result {0.5 0}

skip complex-numbers-19
test complex-numbers-19 "Arithmetic: Division: Divide numbers with real and imaginary part" -body {
    set c1 [ComplexNumber new 1 2]
    set c2 [ComplexNumber new 3 4]
    [$c1 div $c2] toList
} -returnCodes ok -match pairOfFloats -result {0.44 0.08}

skip complex-numbers-20
test complex-numbers-20 "Absolute value: Absolute value of a positive purely real number" -body {
    set c [ComplexNumber new 5 0]
    $c abs
} -returnCodes ok -match float -result 5

skip complex-numbers-21
test complex-numbers-21 "Absolute value: Absolute value of a negative purely real number" -body {
    set c [ComplexNumber new -5 0]
    $c abs
} -returnCodes ok -match float -result 5

skip complex-numbers-22
test complex-numbers-22 "Absolute value: Absolute value of a purely imaginary number with positive imaginary part" -body {
    set c [ComplexNumber new 0 5]
    $c abs
} -returnCodes ok -match float -result 5

skip complex-numbers-23
test complex-numbers-23 "Absolute value: Absolute value of a purely imaginary number with negative imaginary part" -body {
    set c [ComplexNumber new 0 -5]
    $c abs
} -returnCodes ok -match float -result 5

skip complex-numbers-24
test complex-numbers-24 "Absolute value: Absolute value of a number with real and imaginary part" -body {
    set c [ComplexNumber new 3 4]
    $c abs
} -returnCodes ok -match float -result 5

skip complex-numbers-25
test complex-numbers-25 "Complex conjugate: Conjugate a purely real number" -body {
    set c [ComplexNumber new 5 0]
    [$c conj] toList
} -returnCodes ok -match pairOfFloats -result {5 0}

skip complex-numbers-26
test complex-numbers-26 "Complex conjugate: Conjugate a purely imaginary number" -body {
    set c [ComplexNumber new 0 5]
    [$c conj] toList
} -returnCodes ok -match pairOfFloats -result {0 -5}

skip complex-numbers-27
test complex-numbers-27 "Complex conjugate: Conjugate a number with real and imaginary part" -body {
    set c [ComplexNumber new 1 1]
    [$c conj] toList
} -returnCodes ok -match pairOfFloats -result {1 -1}

# Complex exponential function

set pi [expr {atan(1) * 4}]
set e  [expr {exp(1)}]
set ln2 [expr {log(2)}]

skip complex-numbers-28
test complex-numbers-28 "Complex exponential function: Euler's identity/formula" -body {
    set c [ComplexNumber new 0 $pi]
    [$c exp] toList
} -returnCodes ok -match pairOfFloats -result {-1 0}

skip complex-numbers-29
test complex-numbers-29 "Complex exponential function: Exponential of 0" -body {
    set c [ComplexNumber new 0 0]
    [$c exp] toList
} -returnCodes ok -match pairOfFloats -result {1 0}

skip complex-numbers-30
test complex-numbers-30 "Complex exponential function: Exponential of a purely real number" -body {
    set c [ComplexNumber new 1 0]
    [$c exp] toList
} -returnCodes ok -match pairOfFloats -result [list $e 0]

skip complex-numbers-31
test complex-numbers-31 "Complex exponential function: Exponential of a number with real and imaginary part" -body {
    set c [ComplexNumber new $ln2 $pi]
    [$c exp] toList
} -returnCodes ok -match pairOfFloats -result {-2 0}

skip complex-numbers-32
test complex-numbers-32 "Complex exponential function: Exponential resulting in a number with real and imaginary part" -body {
    set c [ComplexNumber new [expr {$ln2 / 2}] [expr {$pi / 4}]]
    [$c exp] toList
} -returnCodes ok -match pairOfFloats -result {1 1}


############################################################
# Operations between real numbers and complex numbers.
#
# These tests require you to write new functions for the
# `expr` command.  The new functions will take two arguments
# (a real number and a complex number, in some order) and
# return a complex number.
#
# See https://www.tcl-lang.org/man/tcl8.6/TclCmd/mathfunc.htm

skip complex-numbers-33
test complex-numbers-33 "Operations between real numbers and complex numbers: Add real number to complex number" -body {
    set c [ComplexNumber new 1 2]
    set r 5
    set d [expr {cr_add($c, $r)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {6 2}

skip complex-numbers-34
test complex-numbers-34 "Operations between real numbers and complex numbers: Add complex number to real number" -body {
    set r 5
    set c [ComplexNumber new 1 2]
    set d [expr {cr_add($r, $c)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {6 2}

skip complex-numbers-35
test complex-numbers-35 "Operations between real numbers and complex numbers: Subtract real number from complex number" -body {
    set c [ComplexNumber new 5 7]
    set r 4
    set d [expr {cr_sub($c, $r)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {1 7}

skip complex-numbers-36
test complex-numbers-36 "Operations between real numbers and complex numbers: Subtract complex number from real number" -body {
    set r 4
    set c [ComplexNumber new 5 7]
    set d [expr {cr_sub($r, $c)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {-1 -7}

skip complex-numbers-37
test complex-numbers-37 "Operations between real numbers and complex numbers: Multiply complex number by real number" -body {
    set c [ComplexNumber new 2 5]
    set r 5
    set d [expr {cr_mul($c, $r)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {10 25}

skip complex-numbers-38
test complex-numbers-38 "Operations between real numbers and complex numbers: Multiply real number by complex number" -body {
    set r 5
    set c [ComplexNumber new 2 5]
    set d [expr {cr_mul($r, $c)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {10 25}

skip complex-numbers-39
test complex-numbers-39 "Operations between real numbers and complex numbers: Divide complex number by real number" -body {
    set c [ComplexNumber new 10 100]
    set r 10
    set d [expr {cr_div($c, $r)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {1 10}

skip complex-numbers-40
test complex-numbers-40 "Operations between real numbers and complex numbers: Divide real number by complex number" -body {
    set r 5
    set c [ComplexNumber new 1 1]
    set d [expr {cr_div($r, $c)}]
    $d toList
} -returnCodes ok -match pairOfFloats -result {2.5 -2.5}


cleanupTests
