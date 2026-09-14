#!/usr/bin/env tclsh
# generated: 2026-07-18T18:43:39Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "meetup.tcl"


test meetup-1 "when teenth Monday is the 13th, the first day of the teenth week" -body {
    meetup 2013 5 teenth Monday
} -returnCodes ok -result 2013-05-13

skip meetup-2
test meetup-2 "when teenth Monday is the 19th, the last day of the teenth week" -body {
    meetup 2013 8 teenth Monday
} -returnCodes ok -result 2013-08-19

skip meetup-3
test meetup-3 "when teenth Monday is some day in the middle of the teenth week" -body {
    meetup 2013 9 teenth Monday
} -returnCodes ok -result 2013-09-16

skip meetup-4
test meetup-4 "when teenth Tuesday is the 19th, the last day of the teenth week" -body {
    meetup 2013 3 teenth Tuesday
} -returnCodes ok -result 2013-03-19

skip meetup-5
test meetup-5 "when teenth Tuesday is some day in the middle of the teenth week" -body {
    meetup 2013 4 teenth Tuesday
} -returnCodes ok -result 2013-04-16

skip meetup-6
test meetup-6 "when teenth Tuesday is the 13th, the first day of the teenth week" -body {
    meetup 2013 8 teenth Tuesday
} -returnCodes ok -result 2013-08-13

skip meetup-7
test meetup-7 "when teenth Wednesday is some day in the middle of the teenth week" -body {
    meetup 2013 1 teenth Wednesday
} -returnCodes ok -result 2013-01-16

skip meetup-8
test meetup-8 "when teenth Wednesday is the 13th, the first day of the teenth week" -body {
    meetup 2013 2 teenth Wednesday
} -returnCodes ok -result 2013-02-13

skip meetup-9
test meetup-9 "when teenth Wednesday is the 19th, the last day of the teenth week" -body {
    meetup 2013 6 teenth Wednesday
} -returnCodes ok -result 2013-06-19

skip meetup-10
test meetup-10 "when teenth Thursday is some day in the middle of the teenth week" -body {
    meetup 2013 5 teenth Thursday
} -returnCodes ok -result 2013-05-16

skip meetup-11
test meetup-11 "when teenth Thursday is the 13th, the first day of the teenth week" -body {
    meetup 2013 6 teenth Thursday
} -returnCodes ok -result 2013-06-13

skip meetup-12
test meetup-12 "when teenth Thursday is the 19th, the last day of the teenth week" -body {
    meetup 2013 9 teenth Thursday
} -returnCodes ok -result 2013-09-19

skip meetup-13
test meetup-13 "when teenth Friday is the 19th, the last day of the teenth week" -body {
    meetup 2013 4 teenth Friday
} -returnCodes ok -result 2013-04-19

skip meetup-14
test meetup-14 "when teenth Friday is some day in the middle of the teenth week" -body {
    meetup 2013 8 teenth Friday
} -returnCodes ok -result 2013-08-16

skip meetup-15
test meetup-15 "when teenth Friday is the 13th, the first day of the teenth week" -body {
    meetup 2013 9 teenth Friday
} -returnCodes ok -result 2013-09-13

skip meetup-16
test meetup-16 "when teenth Saturday is some day in the middle of the teenth week" -body {
    meetup 2013 2 teenth Saturday
} -returnCodes ok -result 2013-02-16

skip meetup-17
test meetup-17 "when teenth Saturday is the 13th, the first day of the teenth week" -body {
    meetup 2013 4 teenth Saturday
} -returnCodes ok -result 2013-04-13

skip meetup-18
test meetup-18 "when teenth Saturday is the 19th, the last day of the teenth week" -body {
    meetup 2013 10 teenth Saturday
} -returnCodes ok -result 2013-10-19

skip meetup-19
test meetup-19 "when teenth Sunday is the 19th, the last day of the teenth week" -body {
    meetup 2013 5 teenth Sunday
} -returnCodes ok -result 2013-05-19

skip meetup-20
test meetup-20 "when teenth Sunday is some day in the middle of the teenth week" -body {
    meetup 2013 6 teenth Sunday
} -returnCodes ok -result 2013-06-16

skip meetup-21
test meetup-21 "when teenth Sunday is the 13th, the first day of the teenth week" -body {
    meetup 2013 10 teenth Sunday
} -returnCodes ok -result 2013-10-13

skip meetup-22
test meetup-22 "when first Monday is some day in the middle of the first week" -body {
    meetup 2013 3 first Monday
} -returnCodes ok -result 2013-03-04

skip meetup-23
test meetup-23 "when first Monday is the 1st, the first day of the first week" -body {
    meetup 2013 4 first Monday
} -returnCodes ok -result 2013-04-01

skip meetup-24
test meetup-24 "when first Tuesday is the 7th, the last day of the first week" -body {
    meetup 2013 5 first Tuesday
} -returnCodes ok -result 2013-05-07

skip meetup-25
test meetup-25 "when first Tuesday is some day in the middle of the first week" -body {
    meetup 2013 6 first Tuesday
} -returnCodes ok -result 2013-06-04

skip meetup-26
test meetup-26 "when first Wednesday is some day in the middle of the first week" -body {
    meetup 2013 7 first Wednesday
} -returnCodes ok -result 2013-07-03

skip meetup-27
test meetup-27 "when first Wednesday is the 7th, the last day of the first week" -body {
    meetup 2013 8 first Wednesday
} -returnCodes ok -result 2013-08-07

skip meetup-28
test meetup-28 "when first Thursday is some day in the middle of the first week" -body {
    meetup 2013 9 first Thursday
} -returnCodes ok -result 2013-09-05

skip meetup-29
test meetup-29 "when first Thursday is another day in the middle of the first week" -body {
    meetup 2013 10 first Thursday
} -returnCodes ok -result 2013-10-03

skip meetup-30
test meetup-30 "when first Friday is the 1st, the first day of the first week" -body {
    meetup 2013 11 first Friday
} -returnCodes ok -result 2013-11-01

skip meetup-31
test meetup-31 "when first Friday is some day in the middle of the first week" -body {
    meetup 2013 12 first Friday
} -returnCodes ok -result 2013-12-06

skip meetup-32
test meetup-32 "when first Saturday is some day in the middle of the first week" -body {
    meetup 2013 1 first Saturday
} -returnCodes ok -result 2013-01-05

skip meetup-33
test meetup-33 "when first Saturday is another day in the middle of the first week" -body {
    meetup 2013 2 first Saturday
} -returnCodes ok -result 2013-02-02

skip meetup-34
test meetup-34 "when first Sunday is some day in the middle of the first week" -body {
    meetup 2013 3 first Sunday
} -returnCodes ok -result 2013-03-03

skip meetup-35
test meetup-35 "when first Sunday is the 7th, the last day of the first week" -body {
    meetup 2013 4 first Sunday
} -returnCodes ok -result 2013-04-07

skip meetup-36
test meetup-36 "when second Monday is some day in the middle of the second week" -body {
    meetup 2013 3 second Monday
} -returnCodes ok -result 2013-03-11

skip meetup-37
test meetup-37 "when second Monday is the 8th, the first day of the second week" -body {
    meetup 2013 4 second Monday
} -returnCodes ok -result 2013-04-08

skip meetup-38
test meetup-38 "when second Tuesday is the 14th, the last day of the second week" -body {
    meetup 2013 5 second Tuesday
} -returnCodes ok -result 2013-05-14

skip meetup-39
test meetup-39 "when second Tuesday is some day in the middle of the second week" -body {
    meetup 2013 6 second Tuesday
} -returnCodes ok -result 2013-06-11

skip meetup-40
test meetup-40 "when second Wednesday is some day in the middle of the second week" -body {
    meetup 2013 7 second Wednesday
} -returnCodes ok -result 2013-07-10

skip meetup-41
test meetup-41 "when second Wednesday is the 14th, the last day of the second week" -body {
    meetup 2013 8 second Wednesday
} -returnCodes ok -result 2013-08-14

skip meetup-42
test meetup-42 "when second Thursday is some day in the middle of the second week" -body {
    meetup 2013 9 second Thursday
} -returnCodes ok -result 2013-09-12

skip meetup-43
test meetup-43 "when second Thursday is another day in the middle of the second week" -body {
    meetup 2013 10 second Thursday
} -returnCodes ok -result 2013-10-10

skip meetup-44
test meetup-44 "when second Friday is the 8th, the first day of the second week" -body {
    meetup 2013 11 second Friday
} -returnCodes ok -result 2013-11-08

skip meetup-45
test meetup-45 "when second Friday is some day in the middle of the second week" -body {
    meetup 2013 12 second Friday
} -returnCodes ok -result 2013-12-13

skip meetup-46
test meetup-46 "when second Saturday is some day in the middle of the second week" -body {
    meetup 2013 1 second Saturday
} -returnCodes ok -result 2013-01-12

skip meetup-47
test meetup-47 "when second Saturday is another day in the middle of the second week" -body {
    meetup 2013 2 second Saturday
} -returnCodes ok -result 2013-02-09

skip meetup-48
test meetup-48 "when second Sunday is some day in the middle of the second week" -body {
    meetup 2013 3 second Sunday
} -returnCodes ok -result 2013-03-10

skip meetup-49
test meetup-49 "when second Sunday is the 14th, the last day of the second week" -body {
    meetup 2013 4 second Sunday
} -returnCodes ok -result 2013-04-14

skip meetup-50
test meetup-50 "when third Monday is some day in the middle of the third week" -body {
    meetup 2013 3 third Monday
} -returnCodes ok -result 2013-03-18

skip meetup-51
test meetup-51 "when third Monday is the 15th, the first day of the third week" -body {
    meetup 2013 4 third Monday
} -returnCodes ok -result 2013-04-15

skip meetup-52
test meetup-52 "when third Tuesday is the 21st, the last day of the third week" -body {
    meetup 2013 5 third Tuesday
} -returnCodes ok -result 2013-05-21

skip meetup-53
test meetup-53 "when third Tuesday is some day in the middle of the third week" -body {
    meetup 2013 6 third Tuesday
} -returnCodes ok -result 2013-06-18

skip meetup-54
test meetup-54 "when third Wednesday is some day in the middle of the third week" -body {
    meetup 2013 7 third Wednesday
} -returnCodes ok -result 2013-07-17

skip meetup-55
test meetup-55 "when third Wednesday is the 21st, the last day of the third week" -body {
    meetup 2013 8 third Wednesday
} -returnCodes ok -result 2013-08-21

skip meetup-56
test meetup-56 "when third Thursday is some day in the middle of the third week" -body {
    meetup 2013 9 third Thursday
} -returnCodes ok -result 2013-09-19

skip meetup-57
test meetup-57 "when third Thursday is another day in the middle of the third week" -body {
    meetup 2013 10 third Thursday
} -returnCodes ok -result 2013-10-17

skip meetup-58
test meetup-58 "when third Friday is the 15th, the first day of the third week" -body {
    meetup 2013 11 third Friday
} -returnCodes ok -result 2013-11-15

skip meetup-59
test meetup-59 "when third Friday is some day in the middle of the third week" -body {
    meetup 2013 12 third Friday
} -returnCodes ok -result 2013-12-20

skip meetup-60
test meetup-60 "when third Saturday is some day in the middle of the third week" -body {
    meetup 2013 1 third Saturday
} -returnCodes ok -result 2013-01-19

skip meetup-61
test meetup-61 "when third Saturday is another day in the middle of the third week" -body {
    meetup 2013 2 third Saturday
} -returnCodes ok -result 2013-02-16

skip meetup-62
test meetup-62 "when third Sunday is some day in the middle of the third week" -body {
    meetup 2013 3 third Sunday
} -returnCodes ok -result 2013-03-17

skip meetup-63
test meetup-63 "when third Sunday is the 21st, the last day of the third week" -body {
    meetup 2013 4 third Sunday
} -returnCodes ok -result 2013-04-21

skip meetup-64
test meetup-64 "when fourth Monday is some day in the middle of the fourth week" -body {
    meetup 2013 3 fourth Monday
} -returnCodes ok -result 2013-03-25

skip meetup-65
test meetup-65 "when fourth Monday is the 22nd, the first day of the fourth week" -body {
    meetup 2013 4 fourth Monday
} -returnCodes ok -result 2013-04-22

skip meetup-66
test meetup-66 "when fourth Tuesday is the 28th, the last day of the fourth week" -body {
    meetup 2013 5 fourth Tuesday
} -returnCodes ok -result 2013-05-28

skip meetup-67
test meetup-67 "when fourth Tuesday is some day in the middle of the fourth week" -body {
    meetup 2013 6 fourth Tuesday
} -returnCodes ok -result 2013-06-25

skip meetup-68
test meetup-68 "when fourth Wednesday is some day in the middle of the fourth week" -body {
    meetup 2013 7 fourth Wednesday
} -returnCodes ok -result 2013-07-24

skip meetup-69
test meetup-69 "when fourth Wednesday is the 28th, the last day of the fourth week" -body {
    meetup 2013 8 fourth Wednesday
} -returnCodes ok -result 2013-08-28

skip meetup-70
test meetup-70 "when fourth Thursday is some day in the middle of the fourth week" -body {
    meetup 2013 9 fourth Thursday
} -returnCodes ok -result 2013-09-26

skip meetup-71
test meetup-71 "when fourth Thursday is another day in the middle of the fourth week" -body {
    meetup 2013 10 fourth Thursday
} -returnCodes ok -result 2013-10-24

skip meetup-72
test meetup-72 "when fourth Friday is the 22nd, the first day of the fourth week" -body {
    meetup 2013 11 fourth Friday
} -returnCodes ok -result 2013-11-22

skip meetup-73
test meetup-73 "when fourth Friday is some day in the middle of the fourth week" -body {
    meetup 2013 12 fourth Friday
} -returnCodes ok -result 2013-12-27

skip meetup-74
test meetup-74 "when fourth Saturday is some day in the middle of the fourth week" -body {
    meetup 2013 1 fourth Saturday
} -returnCodes ok -result 2013-01-26

skip meetup-75
test meetup-75 "when fourth Saturday is another day in the middle of the fourth week" -body {
    meetup 2013 2 fourth Saturday
} -returnCodes ok -result 2013-02-23

skip meetup-76
test meetup-76 "when fourth Sunday is some day in the middle of the fourth week" -body {
    meetup 2013 3 fourth Sunday
} -returnCodes ok -result 2013-03-24

skip meetup-77
test meetup-77 "when fourth Sunday is the 28th, the last day of the fourth week" -body {
    meetup 2013 4 fourth Sunday
} -returnCodes ok -result 2013-04-28

skip meetup-78
test meetup-78 "last Monday in a month with four Mondays" -body {
    meetup 2013 3 last Monday
} -returnCodes ok -result 2013-03-25

skip meetup-79
test meetup-79 "last Monday in a month with five Mondays" -body {
    meetup 2013 4 last Monday
} -returnCodes ok -result 2013-04-29

skip meetup-80
test meetup-80 "last Tuesday in a month with four Tuesdays" -body {
    meetup 2013 5 last Tuesday
} -returnCodes ok -result 2013-05-28

skip meetup-81
test meetup-81 "last Tuesday in another month with four Tuesdays" -body {
    meetup 2013 6 last Tuesday
} -returnCodes ok -result 2013-06-25

skip meetup-82
test meetup-82 "last Wednesday in a month with five Wednesdays" -body {
    meetup 2013 7 last Wednesday
} -returnCodes ok -result 2013-07-31

skip meetup-83
test meetup-83 "last Wednesday in a month with four Wednesdays" -body {
    meetup 2013 8 last Wednesday
} -returnCodes ok -result 2013-08-28

skip meetup-84
test meetup-84 "last Thursday in a month with four Thursdays" -body {
    meetup 2013 9 last Thursday
} -returnCodes ok -result 2013-09-26

skip meetup-85
test meetup-85 "last Thursday in a month with five Thursdays" -body {
    meetup 2013 10 last Thursday
} -returnCodes ok -result 2013-10-31

skip meetup-86
test meetup-86 "last Friday in a month with five Fridays" -body {
    meetup 2013 11 last Friday
} -returnCodes ok -result 2013-11-29

skip meetup-87
test meetup-87 "last Friday in a month with four Fridays" -body {
    meetup 2013 12 last Friday
} -returnCodes ok -result 2013-12-27

skip meetup-88
test meetup-88 "last Saturday in a month with four Saturdays" -body {
    meetup 2013 1 last Saturday
} -returnCodes ok -result 2013-01-26

skip meetup-89
test meetup-89 "last Saturday in another month with four Saturdays" -body {
    meetup 2013 2 last Saturday
} -returnCodes ok -result 2013-02-23

skip meetup-90
test meetup-90 "last Sunday in a month with five Sundays" -body {
    meetup 2013 3 last Sunday
} -returnCodes ok -result 2013-03-31

skip meetup-91
test meetup-91 "last Sunday in a month with four Sundays" -body {
    meetup 2013 4 last Sunday
} -returnCodes ok -result 2013-04-28

skip meetup-92
test meetup-92 "when last Wednesday in February in a leap year is the 29th" -body {
    meetup 2012 2 last Wednesday
} -returnCodes ok -result 2012-02-29

skip meetup-93
test meetup-93 "last Wednesday in December that is also the last day of the year" -body {
    meetup 2014 12 last Wednesday
} -returnCodes ok -result 2014-12-31

skip meetup-94
test meetup-94 "when last Sunday in February in a non-leap year is not the 29th" -body {
    meetup 2015 2 last Sunday
} -returnCodes ok -result 2015-02-22

skip meetup-95
test meetup-95 "when first Friday is the 7th, the last day of the first week" -body {
    meetup 2012 12 first Friday
} -returnCodes ok -result 2012-12-07


cleanupTests
