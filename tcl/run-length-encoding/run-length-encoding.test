#!/usr/bin/env tclsh
# generated: 2026-07-23T16:08:56Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "run-length-encoding.tcl"


test run-length-encoding-1 "run-length encode a string: empty string" -body {
    encode ""
} -returnCodes ok -result ""

skip run-length-encoding-2
test run-length-encoding-2 "run-length encode a string: single characters only are encoded without count" -body {
    encode "XYZ"
} -returnCodes ok -result "XYZ"

skip run-length-encoding-3
test run-length-encoding-3 "run-length encode a string: string with no single characters" -body {
    encode "AABBBCCCC"
} -returnCodes ok -result "2A3B4C"

skip run-length-encoding-4
test run-length-encoding-4 "run-length encode a string: single characters mixed with repeated characters" -body {
    encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
} -returnCodes ok -result "12WB12W3B24WB"

skip run-length-encoding-5
test run-length-encoding-5 "run-length encode a string: multiple whitespace mixed in string" -body {
    encode "  hsqq qww  "
} -returnCodes ok -result "2 hs2q q2w2 "

skip run-length-encoding-6
test run-length-encoding-6 "run-length encode a string: lowercase characters" -body {
    encode "aabbbcccc"
} -returnCodes ok -result "2a3b4c"

skip run-length-encoding-7
test run-length-encoding-7 "run-length decode a string: empty string" -body {
    decode ""
} -returnCodes ok -result ""

skip run-length-encoding-8
test run-length-encoding-8 "run-length decode a string: single characters only" -body {
    decode "XYZ"
} -returnCodes ok -result "XYZ"

skip run-length-encoding-9
test run-length-encoding-9 "run-length decode a string: string with no single characters" -body {
    decode "2A3B4C"
} -returnCodes ok -result "AABBBCCCC"

skip run-length-encoding-10
test run-length-encoding-10 "run-length decode a string: single characters with repeated characters" -body {
    decode "12WB12W3B24WB"
} -returnCodes ok -result "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"

skip run-length-encoding-11
test run-length-encoding-11 "run-length decode a string: multiple whitespace mixed in string" -body {
    decode "2 hs2q q2w2 "
} -returnCodes ok -result "  hsqq qww  "

skip run-length-encoding-12
test run-length-encoding-12 "run-length decode a string: lowercase string" -body {
    decode "2a3b4c"
} -returnCodes ok -result "aabbbcccc"

skip run-length-encoding-13
test run-length-encoding-13 "encode and then decode: encode followed by decode gives original string" -body {
    decode [encode "zzz ZZ  zZ"]
} -returnCodes ok -result "zzz ZZ  zZ"


cleanupTests
