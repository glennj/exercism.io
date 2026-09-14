#!/usr/bin/env tclsh
# generated: 2026-07-16T21:34:42Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "allergies.tcl"


test allergies-1 "testing for eggs allergy: not allergic to anything" -body {
    allergicTo eggs 0
} -returnCodes ok -match boolean -result false

skip allergies-2
test allergies-2 "testing for eggs allergy: allergic only to eggs" -body {
    allergicTo eggs 1
} -returnCodes ok -match boolean -result true

skip allergies-3
test allergies-3 "testing for eggs allergy: allergic to eggs and something else" -body {
    allergicTo eggs 3
} -returnCodes ok -match boolean -result true

skip allergies-4
test allergies-4 "testing for eggs allergy: allergic to something, but not eggs" -body {
    allergicTo eggs 2
} -returnCodes ok -match boolean -result false

skip allergies-5
test allergies-5 "testing for eggs allergy: allergic to everything" -body {
    allergicTo eggs 255
} -returnCodes ok -match boolean -result true

skip allergies-6
test allergies-6 "testing for peanuts allergy: not allergic to anything" -body {
    allergicTo peanuts 0
} -returnCodes ok -match boolean -result false

skip allergies-7
test allergies-7 "testing for peanuts allergy: allergic only to peanuts" -body {
    allergicTo peanuts 2
} -returnCodes ok -match boolean -result true

skip allergies-8
test allergies-8 "testing for peanuts allergy: allergic to peanuts and something else" -body {
    allergicTo peanuts 7
} -returnCodes ok -match boolean -result true

skip allergies-9
test allergies-9 "testing for peanuts allergy: allergic to something, but not peanuts" -body {
    allergicTo peanuts 5
} -returnCodes ok -match boolean -result false

skip allergies-10
test allergies-10 "testing for peanuts allergy: allergic to everything" -body {
    allergicTo peanuts 255
} -returnCodes ok -match boolean -result true

skip allergies-11
test allergies-11 "testing for shellfish allergy: not allergic to anything" -body {
    allergicTo shellfish 0
} -returnCodes ok -match boolean -result false

skip allergies-12
test allergies-12 "testing for shellfish allergy: allergic only to shellfish" -body {
    allergicTo shellfish 4
} -returnCodes ok -match boolean -result true

skip allergies-13
test allergies-13 "testing for shellfish allergy: allergic to shellfish and something else" -body {
    allergicTo shellfish 14
} -returnCodes ok -match boolean -result true

skip allergies-14
test allergies-14 "testing for shellfish allergy: allergic to something, but not shellfish" -body {
    allergicTo shellfish 10
} -returnCodes ok -match boolean -result false

skip allergies-15
test allergies-15 "testing for shellfish allergy: allergic to everything" -body {
    allergicTo shellfish 255
} -returnCodes ok -match boolean -result true

skip allergies-16
test allergies-16 "testing for strawberries allergy: not allergic to anything" -body {
    allergicTo strawberries 0
} -returnCodes ok -match boolean -result false

skip allergies-17
test allergies-17 "testing for strawberries allergy: allergic only to strawberries" -body {
    allergicTo strawberries 8
} -returnCodes ok -match boolean -result true

skip allergies-18
test allergies-18 "testing for strawberries allergy: allergic to strawberries and something else" -body {
    allergicTo strawberries 28
} -returnCodes ok -match boolean -result true

skip allergies-19
test allergies-19 "testing for strawberries allergy: allergic to something, but not strawberries" -body {
    allergicTo strawberries 20
} -returnCodes ok -match boolean -result false

skip allergies-20
test allergies-20 "testing for strawberries allergy: allergic to everything" -body {
    allergicTo strawberries 255
} -returnCodes ok -match boolean -result true

skip allergies-21
test allergies-21 "testing for tomatoes allergy: not allergic to anything" -body {
    allergicTo tomatoes 0
} -returnCodes ok -match boolean -result false

skip allergies-22
test allergies-22 "testing for tomatoes allergy: allergic only to tomatoes" -body {
    allergicTo tomatoes 16
} -returnCodes ok -match boolean -result true

skip allergies-23
test allergies-23 "testing for tomatoes allergy: allergic to tomatoes and something else" -body {
    allergicTo tomatoes 56
} -returnCodes ok -match boolean -result true

skip allergies-24
test allergies-24 "testing for tomatoes allergy: allergic to something, but not tomatoes" -body {
    allergicTo tomatoes 40
} -returnCodes ok -match boolean -result false

skip allergies-25
test allergies-25 "testing for tomatoes allergy: allergic to everything" -body {
    allergicTo tomatoes 255
} -returnCodes ok -match boolean -result true

skip allergies-26
test allergies-26 "testing for chocolate allergy: not allergic to anything" -body {
    allergicTo chocolate 0
} -returnCodes ok -match boolean -result false

skip allergies-27
test allergies-27 "testing for chocolate allergy: allergic only to chocolate" -body {
    allergicTo chocolate 32
} -returnCodes ok -match boolean -result true

skip allergies-28
test allergies-28 "testing for chocolate allergy: allergic to chocolate and something else" -body {
    allergicTo chocolate 112
} -returnCodes ok -match boolean -result true

skip allergies-29
test allergies-29 "testing for chocolate allergy: allergic to something, but not chocolate" -body {
    allergicTo chocolate 80
} -returnCodes ok -match boolean -result false

skip allergies-30
test allergies-30 "testing for chocolate allergy: allergic to everything" -body {
    allergicTo chocolate 255
} -returnCodes ok -match boolean -result true

skip allergies-31
test allergies-31 "testing for pollen allergy: not allergic to anything" -body {
    allergicTo pollen 0
} -returnCodes ok -match boolean -result false

skip allergies-32
test allergies-32 "testing for pollen allergy: allergic only to pollen" -body {
    allergicTo pollen 64
} -returnCodes ok -match boolean -result true

skip allergies-33
test allergies-33 "testing for pollen allergy: allergic to pollen and something else" -body {
    allergicTo pollen 224
} -returnCodes ok -match boolean -result true

skip allergies-34
test allergies-34 "testing for pollen allergy: allergic to something, but not pollen" -body {
    allergicTo pollen 160
} -returnCodes ok -match boolean -result false

skip allergies-35
test allergies-35 "testing for pollen allergy: allergic to everything" -body {
    allergicTo pollen 255
} -returnCodes ok -match boolean -result true

skip allergies-36
test allergies-36 "testing for cats allergy: not allergic to anything" -body {
    allergicTo cats 0
} -returnCodes ok -match boolean -result false

skip allergies-37
test allergies-37 "testing for cats allergy: allergic only to cats" -body {
    allergicTo cats 128
} -returnCodes ok -match boolean -result true

skip allergies-38
test allergies-38 "testing for cats allergy: allergic to cats and something else" -body {
    allergicTo cats 192
} -returnCodes ok -match boolean -result true

skip allergies-39
test allergies-39 "testing for cats allergy: allergic to something, but not cats" -body {
    allergicTo cats 64
} -returnCodes ok -match boolean -result false

skip allergies-40
test allergies-40 "testing for cats allergy: allergic to everything" -body {
    allergicTo cats 255
} -returnCodes ok -match boolean -result true

skip allergies-41
test allergies-41 "list when:: no allergies" -body {
    listAllergies 0
} -returnCodes ok -result {}

skip allergies-42
test allergies-42 "list when:: just eggs" -body {
    listAllergies 1
} -returnCodes ok -result {eggs}

skip allergies-43
test allergies-43 "list when:: just peanuts" -body {
    listAllergies 2
} -returnCodes ok -result {peanuts}

skip allergies-44
test allergies-44 "list when:: just strawberries" -body {
    listAllergies 8
} -returnCodes ok -result {strawberries}

skip allergies-45
test allergies-45 "list when:: eggs and peanuts" -body {
    listAllergies 3
} -returnCodes ok -result {eggs peanuts}

skip allergies-46
test allergies-46 "list when:: more than eggs but not peanuts" -body {
    listAllergies 5
} -returnCodes ok -result {eggs shellfish}

skip allergies-47
test allergies-47 "list when:: lots of stuff" -body {
    listAllergies 248
} -returnCodes ok -result {strawberries tomatoes chocolate pollen cats}

skip allergies-48
test allergies-48 "list when:: everything" -body {
    listAllergies 255
} -returnCodes ok -result {eggs peanuts shellfish strawberries tomatoes chocolate pollen cats}

skip allergies-49
test allergies-49 "list when:: no allergen score parts" -body {
    listAllergies 509
} -returnCodes ok -result {eggs shellfish strawberries tomatoes chocolate pollen cats}

skip allergies-50
test allergies-50 "list when:: no allergen score parts without highest valid score" -body {
    listAllergies 257
} -returnCodes ok -result {eggs}


cleanupTests
