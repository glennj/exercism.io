#!/usr/bin/env tclsh
# generated: 2026-07-24T01:37:55Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "secret-handshake.tcl"


test secret-handshake-1 "wink for 1" -body {
    secretHandshake 1
} -returnCodes ok -match orderedLists -result {"wink"}

skip secret-handshake-2
test secret-handshake-2 "double blink for 10" -body {
    secretHandshake 2
} -returnCodes ok -match orderedLists -result {"double blink"}

skip secret-handshake-3
test secret-handshake-3 "close your eyes for 100" -body {
    secretHandshake 4
} -returnCodes ok -match orderedLists -result {"close your eyes"}

skip secret-handshake-4
test secret-handshake-4 "jump for 1000" -body {
    secretHandshake 8
} -returnCodes ok -match orderedLists -result {"jump"}

skip secret-handshake-5
test secret-handshake-5 "combine two actions" -body {
    secretHandshake 3
} -returnCodes ok -match orderedLists -result {"wink" "double blink"}

skip secret-handshake-6
test secret-handshake-6 "reverse two actions" -body {
    secretHandshake 19
} -returnCodes ok -match orderedLists -result {"double blink" "wink"}

skip secret-handshake-7
test secret-handshake-7 "reversing one action gives the same action" -body {
    secretHandshake 24
} -returnCodes ok -match orderedLists -result {"jump"}

skip secret-handshake-8
test secret-handshake-8 "reversing no actions still gives no actions" -body {
    secretHandshake 16
} -returnCodes ok -match orderedLists -result {}

skip secret-handshake-9
test secret-handshake-9 "all possible actions" -body {
    secretHandshake 15
} -returnCodes ok -match orderedLists -result {"wink" "double blink" "close your eyes" "jump"}

skip secret-handshake-10
test secret-handshake-10 "reverse all possible actions" -body {
    secretHandshake 31
} -returnCodes ok -match orderedLists -result {"jump" "close your eyes" "double blink" "wink"}

skip secret-handshake-11
test secret-handshake-11 "do nothing for zero" -body {
    secretHandshake 0
} -returnCodes ok -match orderedLists -result {}


cleanupTests
