#!/usr/bin/env tclsh
# generated: 2026-07-23T12:02:04Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "raindrops.tcl"


test raindrops-1 "the sound for 1 is 1" -body {
    raindrops 1
} -returnCodes ok -result 1

skip raindrops-2
test raindrops-2 "the sound for 3 is Pling" -body {
    raindrops 3
} -returnCodes ok -result Pling

skip raindrops-3
test raindrops-3 "the sound for 5 is Plang" -body {
    raindrops 5
} -returnCodes ok -result Plang

skip raindrops-4
test raindrops-4 "the sound for 7 is Plong" -body {
    raindrops 7
} -returnCodes ok -result Plong

skip raindrops-5
test raindrops-5 "the sound for 6 is Pling as it has a factor 3" -body {
    raindrops 6
} -returnCodes ok -result Pling

skip raindrops-6
test raindrops-6 "2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base" -body {
    raindrops 8
} -returnCodes ok -result 8

skip raindrops-7
test raindrops-7 "the sound for 9 is Pling as it has a factor 3" -body {
    raindrops 9
} -returnCodes ok -result Pling

skip raindrops-8
test raindrops-8 "the sound for 10 is Plang as it has a factor 5" -body {
    raindrops 10
} -returnCodes ok -result Plang

skip raindrops-9
test raindrops-9 "the sound for 14 is Plong as it has a factor of 7" -body {
    raindrops 14
} -returnCodes ok -result Plong

skip raindrops-10
test raindrops-10 "the sound for 15 is PlingPlang as it has factors 3 and 5" -body {
    raindrops 15
} -returnCodes ok -result PlingPlang

skip raindrops-11
test raindrops-11 "the sound for 21 is PlingPlong as it has factors 3 and 7" -body {
    raindrops 21
} -returnCodes ok -result PlingPlong

skip raindrops-12
test raindrops-12 "the sound for 25 is Plang as it has a factor 5" -body {
    raindrops 25
} -returnCodes ok -result Plang

skip raindrops-13
test raindrops-13 "the sound for 27 is Pling as it has a factor 3" -body {
    raindrops 27
} -returnCodes ok -result Pling

skip raindrops-14
test raindrops-14 "the sound for 35 is PlangPlong as it has factors 5 and 7" -body {
    raindrops 35
} -returnCodes ok -result PlangPlong

skip raindrops-15
test raindrops-15 "the sound for 49 is Plong as it has a factor 7" -body {
    raindrops 49
} -returnCodes ok -result Plong

skip raindrops-16
test raindrops-16 "the sound for 52 is 52" -body {
    raindrops 52
} -returnCodes ok -result 52

skip raindrops-17
test raindrops-17 "the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7" -body {
    raindrops 105
} -returnCodes ok -result PlingPlangPlong

skip raindrops-18
test raindrops-18 "the sound for 3125 is Plang as it has a factor 5" -body {
    raindrops 3125
} -returnCodes ok -result Plang


cleanupTests
