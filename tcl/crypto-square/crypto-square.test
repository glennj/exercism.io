#!/usr/bin/env tclsh
# generated: 2026-07-17T17:50:30Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "crypto-square.tcl"


test crypto-square-1 "empty plaintext results in an empty ciphertext" -body {
    encrypt ""
} -returnCodes ok -result ""

skip crypto-square-2
test crypto-square-2 "normalization results in empty plaintext" -body {
    encrypt "... --- ..."
} -returnCodes ok -result ""

skip crypto-square-3
test crypto-square-3 "Lowercase" -body {
    encrypt "A"
} -returnCodes ok -result "a"

skip crypto-square-4
test crypto-square-4 "Remove spaces" -body {
    encrypt "  b "
} -returnCodes ok -result "b"

skip crypto-square-5
test crypto-square-5 "Remove punctuation" -body {
    encrypt "@1,%!"
} -returnCodes ok -result "1"

skip crypto-square-6
test crypto-square-6 "9 character plaintext results in 3 chunks of 3 characters" -body {
    encrypt "This is fun!"
} -returnCodes ok -result "tsf hiu isn"

skip crypto-square-7
test crypto-square-7 "8 character plaintext results in 3 chunks, the last one with a trailing space" -body {
    encrypt "Chill out."
} -returnCodes ok -result "clu hlt io "

skip crypto-square-8
test crypto-square-8 "54 character plaintext results in 8 chunks, the last two with trailing spaces" -body {
    encrypt "If man was meant to stay on the ground, god would have given us roots."
} -returnCodes ok -result "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "


cleanupTests
