#!/usr/bin/env tclsh
# generated: 2026-07-25T19:20:13Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "grade-school.tcl"


test grade-school-1 "Roster is empty when no student is added" -setup {
    school reset
} -body {
    # no students added
    school roster
} -returnCodes ok -match orderedLists -result {}

skip grade-school-2
test grade-school-2 "Add a student" -setup {
    school reset
} -body {
    school add {{Aimee 2}}
} -returnCodes ok -match orderedLists -result {true}

skip grade-school-3
test grade-school-3 "Student is added to the roster" -setup {
    school reset
} -body {
    school add {{Aimee 2}}
    school roster
} -returnCodes ok -match orderedLists -result {Aimee}

skip grade-school-4
test grade-school-4 "Adding multiple students in the same grade in the roster" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {Paul 2}}
} -returnCodes ok -match orderedLists -result {true true true}

skip grade-school-5
test grade-school-5 "Multiple students in the same grade are added to the roster" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {Paul 2}}
    school roster
} -returnCodes ok -match orderedLists -result {Blair James Paul}

skip grade-school-6
test grade-school-6 "Cannot add student to same grade in the roster more than once" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 2} {Paul 2}}
} -returnCodes ok -match orderedLists -result {true true false true}

skip grade-school-7
test grade-school-7 "Student not added to same grade in the roster more than once" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 2} {Paul 2}}
    school roster
} -returnCodes ok -match orderedLists -result {Blair James Paul}

skip grade-school-8
test grade-school-8 "Adding students in multiple grades" -setup {
    school reset
} -body {
    school add {{Chelsea 3} {Logan 7}}
} -returnCodes ok -match orderedLists -result {true true}

skip grade-school-9
test grade-school-9 "Students in multiple grades are added to the roster" -setup {
    school reset
} -body {
    school add {{Chelsea 3} {Logan 7}}
    school roster
} -returnCodes ok -match orderedLists -result {Chelsea Logan}

skip grade-school-10
test grade-school-10 "Cannot add same student to multiple grades in the roster" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 3} {Paul 3}}
} -returnCodes ok -match orderedLists -result {true true false true}

skip grade-school-11
test grade-school-11 "Student not added to multiple grades in the roster" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 3} {Paul 3}}
    school roster
} -returnCodes ok -match orderedLists -result {Blair James Paul}

skip grade-school-12
test grade-school-12 "Students are sorted by grades in the roster" -setup {
    school reset
} -body {
    school add {{Jim 3} {Peter 2} {Anna 1}}
    school roster
} -returnCodes ok -match orderedLists -result {Anna Peter Jim}

skip grade-school-13
test grade-school-13 "Students are sorted by name in the roster" -setup {
    school reset
} -body {
    school add {{Peter 2} {Zoe 2} {Alex 2}}
    school roster
} -returnCodes ok -match orderedLists -result {Alex Peter Zoe}

skip grade-school-14
test grade-school-14 "Students are sorted by grades and then by name in the roster" -setup {
    school reset
} -body {
    school add {{Peter 2} {Anna 1} {Barb 1} {Zoe 2} {Alex 2} {Jim 3} {Charlie 1}}
    school roster
} -returnCodes ok -match orderedLists -result {Anna Barb Charlie Alex Peter Zoe Jim}

skip grade-school-15
test grade-school-15 "Grade is empty if no students in the roster" -setup {
    school reset
} -body {
    # no students added
    school grade 1
} -returnCodes ok -match orderedLists -result {}

skip grade-school-16
test grade-school-16 "Grade is empty if no students in that grade" -setup {
    school reset
} -body {
    school add {{Peter 2} {Zoe 2} {Alex 2} {Jim 3}}
    school grade 1
} -returnCodes ok -match orderedLists -result {}

skip grade-school-17
test grade-school-17 "Student not added to same grade more than once" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 2} {Paul 2}}
    school grade 2
} -returnCodes ok -match orderedLists -result {Blair James Paul}

skip grade-school-18
test grade-school-18 "Student not added to multiple grades" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 3} {Paul 3}}
    school grade 2
} -returnCodes ok -match orderedLists -result {Blair James}

skip grade-school-19
test grade-school-19 "Student not added to other grade for multiple grades" -setup {
    school reset
} -body {
    school add {{Blair 2} {James 2} {James 3} {Paul 3}}
    school grade 3
} -returnCodes ok -match orderedLists -result {Paul}

skip grade-school-20
test grade-school-20 "Students are sorted by name in a grade" -setup {
    school reset
} -body {
    school add {{Franklin 5} {Bradley 5} {Jeff 1}}
    school grade 5
} -returnCodes ok -match orderedLists -result {Bradley Franklin}


cleanupTests
