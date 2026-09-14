#!/usr/bin/env tclsh
# generated: 2026-07-17T12:52:17Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "clock.tcl"


test clock-1 "Create a new clock with an initial time: on the hour" -body {
    [Clock new 8 0] toString
} -returnCodes ok -result "08:00"

skip clock-2
test clock-2 "Create a new clock with an initial time: past the hour" -body {
    [Clock new 11 9] toString
} -returnCodes ok -result "11:09"

skip clock-3
test clock-3 "Create a new clock with an initial time: midnight is zero hours" -body {
    [Clock new 24 0] toString
} -returnCodes ok -result "00:00"

skip clock-4
test clock-4 "Create a new clock with an initial time: hour rolls over" -body {
    [Clock new 25 0] toString
} -returnCodes ok -result "01:00"

skip clock-5
test clock-5 "Create a new clock with an initial time: hour rolls over continuously" -body {
    [Clock new 100 0] toString
} -returnCodes ok -result "04:00"

skip clock-6
test clock-6 "Create a new clock with an initial time: sixty minutes is next hour" -body {
    [Clock new 1 60] toString
} -returnCodes ok -result "02:00"

skip clock-7
test clock-7 "Create a new clock with an initial time: minutes roll over" -body {
    [Clock new 0 160] toString
} -returnCodes ok -result "02:40"

skip clock-8
test clock-8 "Create a new clock with an initial time: minutes roll over continuously" -body {
    [Clock new 0 1723] toString
} -returnCodes ok -result "04:43"

skip clock-9
test clock-9 "Create a new clock with an initial time: hour and minutes roll over" -body {
    [Clock new 25 160] toString
} -returnCodes ok -result "03:40"

skip clock-10
test clock-10 "Create a new clock with an initial time: hour and minutes roll over continuously" -body {
    [Clock new 201 3001] toString
} -returnCodes ok -result "11:01"

skip clock-11
test clock-11 "Create a new clock with an initial time: hour and minutes roll over to exactly midnight" -body {
    [Clock new 72 8640] toString
} -returnCodes ok -result "00:00"

skip clock-12
test clock-12 "Create a new clock with an initial time: negative hour" -body {
    [Clock new -1 15] toString
} -returnCodes ok -result "23:15"

skip clock-13
test clock-13 "Create a new clock with an initial time: negative hour rolls over" -body {
    [Clock new -25 0] toString
} -returnCodes ok -result "23:00"

skip clock-14
test clock-14 "Create a new clock with an initial time: negative hour rolls over continuously" -body {
    [Clock new -91 0] toString
} -returnCodes ok -result "05:00"

skip clock-15
test clock-15 "Create a new clock with an initial time: negative minutes" -body {
    [Clock new 1 -40] toString
} -returnCodes ok -result "00:20"

skip clock-16
test clock-16 "Create a new clock with an initial time: negative minutes roll over" -body {
    [Clock new 1 -160] toString
} -returnCodes ok -result "22:20"

skip clock-17
test clock-17 "Create a new clock with an initial time: negative minutes roll over continuously" -body {
    [Clock new 1 -4820] toString
} -returnCodes ok -result "16:40"

skip clock-18
test clock-18 "Create a new clock with an initial time: negative sixty minutes is previous hour" -body {
    [Clock new 2 -60] toString
} -returnCodes ok -result "01:00"

skip clock-19
test clock-19 "Create a new clock with an initial time: negative hour and minutes both roll over" -body {
    [Clock new -25 -160] toString
} -returnCodes ok -result "20:20"

skip clock-20
test clock-20 "Create a new clock with an initial time: negative hour and minutes both roll over continuously" -body {
    [Clock new -121 -5810] toString
} -returnCodes ok -result "22:10"

skip clock-21
test clock-21 "Add minutes: add minutes" -body {
    [[Clock new 10 0] add 3] toString
} -returnCodes ok -result "10:03"

skip clock-22
test clock-22 "Add minutes: add no minutes" -body {
    [[Clock new 6 41] add 0] toString
} -returnCodes ok -result "06:41"

skip clock-23
test clock-23 "Add minutes: add to next hour" -body {
    [[Clock new 0 45] add 40] toString
} -returnCodes ok -result "01:25"

skip clock-24
test clock-24 "Add minutes: add more than one hour" -body {
    [[Clock new 10 0] add 61] toString
} -returnCodes ok -result "11:01"

skip clock-25
test clock-25 "Add minutes: add more than two hours with carry" -body {
    [[Clock new 0 45] add 160] toString
} -returnCodes ok -result "03:25"

skip clock-26
test clock-26 "Add minutes: add across midnight" -body {
    [[Clock new 23 59] add 2] toString
} -returnCodes ok -result "00:01"

skip clock-27
test clock-27 "Add minutes: add more than one day (1500 min = 25 hrs)" -body {
    [[Clock new 5 32] add 1500] toString
} -returnCodes ok -result "06:32"

skip clock-28
test clock-28 "Add minutes: add more than two days" -body {
    [[Clock new 1 1] add 3500] toString
} -returnCodes ok -result "11:21"

skip clock-29
test clock-29 "Subtract minutes: subtract minutes" -body {
    [[Clock new 10 3] subtract 3] toString
} -returnCodes ok -result "10:00"

skip clock-30
test clock-30 "Subtract minutes: subtract to previous hour" -body {
    [[Clock new 10 3] subtract 30] toString
} -returnCodes ok -result "09:33"

skip clock-31
test clock-31 "Subtract minutes: subtract more than an hour" -body {
    [[Clock new 10 3] subtract 70] toString
} -returnCodes ok -result "08:53"

skip clock-32
test clock-32 "Subtract minutes: subtract across midnight" -body {
    [[Clock new 0 3] subtract 4] toString
} -returnCodes ok -result "23:59"

skip clock-33
test clock-33 "Subtract minutes: subtract more than two hours" -body {
    [[Clock new 0 0] subtract 160] toString
} -returnCodes ok -result "21:20"

skip clock-34
test clock-34 "Subtract minutes: subtract more than two hours with borrow" -body {
    [[Clock new 6 15] subtract 160] toString
} -returnCodes ok -result "03:35"

skip clock-35
test clock-35 "Subtract minutes: subtract more than one day (1500 min = 25 hrs)" -body {
    [[Clock new 5 32] subtract 1500] toString
} -returnCodes ok -result "04:32"

skip clock-36
test clock-36 "Subtract minutes: subtract more than two days" -body {
    [[Clock new 2 20] subtract 3000] toString
} -returnCodes ok -result "00:20"

skip clock-37
test clock-37 "Compare two clocks for equality: clocks with same time" -body {
    set c1 [Clock new 15 37]
    set c2 [Clock new 15 37]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-38
test clock-38 "Compare two clocks for equality: clocks a minute apart" -body {
    set c1 [Clock new 15 36]
    set c2 [Clock new 15 37]
    $c1 equals $c2
} -returnCodes ok -match boolean -result false

skip clock-39
test clock-39 "Compare two clocks for equality: clocks an hour apart" -body {
    set c1 [Clock new 14 37]
    set c2 [Clock new 15 37]
    $c1 equals $c2
} -returnCodes ok -match boolean -result false

skip clock-40
test clock-40 "Compare two clocks for equality: clocks with hour overflow" -body {
    set c1 [Clock new 10 37]
    set c2 [Clock new 34 37]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-41
test clock-41 "Compare two clocks for equality: clocks with hour overflow by several days" -body {
    set c1 [Clock new 3 11]
    set c2 [Clock new 99 11]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-42
test clock-42 "Compare two clocks for equality: clocks with negative hour" -body {
    set c1 [Clock new 22 40]
    set c2 [Clock new -2 40]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-43
test clock-43 "Compare two clocks for equality: clocks with negative hour that wraps" -body {
    set c1 [Clock new 17 3]
    set c2 [Clock new -31 3]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-44
test clock-44 "Compare two clocks for equality: clocks with negative hour that wraps multiple times" -body {
    set c1 [Clock new 13 49]
    set c2 [Clock new -83 49]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-45
test clock-45 "Compare two clocks for equality: clocks with minute overflow" -body {
    set c1 [Clock new 0 1]
    set c2 [Clock new 0 1441]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-46
test clock-46 "Compare two clocks for equality: clocks with minute overflow by several days" -body {
    set c1 [Clock new 2 2]
    set c2 [Clock new 2 4322]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-47
test clock-47 "Compare two clocks for equality: clocks with negative minute" -body {
    set c1 [Clock new 2 40]
    set c2 [Clock new 3 -20]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-48
test clock-48 "Compare two clocks for equality: clocks with negative minute that wraps" -body {
    set c1 [Clock new 4 10]
    set c2 [Clock new 5 -1490]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-49
test clock-49 "Compare two clocks for equality: clocks with negative minute that wraps multiple times" -body {
    set c1 [Clock new 6 15]
    set c2 [Clock new 6 -4305]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-50
test clock-50 "Compare two clocks for equality: clocks with negative hours and minutes" -body {
    set c1 [Clock new 7 32]
    set c2 [Clock new -12 -268]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-51
test clock-51 "Compare two clocks for equality: clocks with negative hours and minutes that wrap" -body {
    set c1 [Clock new 18 7]
    set c2 [Clock new -54 -11513]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-52
test clock-52 "Compare two clocks for equality: full clock and zeroed clock" -body {
    set c1 [Clock new 24 0]
    set c2 [Clock new 0 0]
    $c1 equals $c2
} -returnCodes ok -match boolean -result true

skip clock-bonus
test clock-bonus "bonus: non-alpha method name" -body {
    set c1 [Clock new 8 1]
    set c2 [Clock new 8 2]
    $c1 == $c2
} -returnCodes ok -match boolean -result false

cleanupTests
