#!/usr/bin/env tclsh
# generated: 2026-07-17T17:57:15Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "bottle-song.tcl"


test bottle-song-1 "verse: single verse: first generic verse" -body {
    bottleSong 10 1
} -returnCodes ok -match orderedLists -result {
    "Ten green bottles hanging on the wall,"
    "Ten green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be nine green bottles hanging on the wall."
}

skip bottle-song-2
test bottle-song-2 "verse: single verse: last generic verse" -body {
    bottleSong 3 1
} -returnCodes ok -match orderedLists -result {
    "Three green bottles hanging on the wall,"
    "Three green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be two green bottles hanging on the wall."
}

skip bottle-song-3
test bottle-song-3 "verse: single verse: verse with 2 bottles" -body {
    bottleSong 2 1
} -returnCodes ok -match orderedLists -result {
    "Two green bottles hanging on the wall,"
    "Two green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be one green bottle hanging on the wall."
}

skip bottle-song-4
test bottle-song-4 "verse: single verse: verse with 1 bottle" -body {
    bottleSong 1 1
} -returnCodes ok -match orderedLists -result {
    "One green bottle hanging on the wall,"
    "One green bottle hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be no green bottles hanging on the wall."
}

skip bottle-song-5
test bottle-song-5 "lyrics: multiple verses: first two verses" -body {
    bottleSong 10 2
} -returnCodes ok -match orderedLists -result {
    "Ten green bottles hanging on the wall,"
    "Ten green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be nine green bottles hanging on the wall."
    ""
    "Nine green bottles hanging on the wall,"
    "Nine green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be eight green bottles hanging on the wall."
}

skip bottle-song-6
test bottle-song-6 "lyrics: multiple verses: last three verses" -body {
    bottleSong 3 3
} -returnCodes ok -match orderedLists -result {
    "Three green bottles hanging on the wall,"
    "Three green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be two green bottles hanging on the wall."
    ""
    "Two green bottles hanging on the wall,"
    "Two green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be one green bottle hanging on the wall."
    ""
    "One green bottle hanging on the wall,"
    "One green bottle hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be no green bottles hanging on the wall."
}

skip bottle-song-7
test bottle-song-7 "lyrics: multiple verses: all verses" -body {
    bottleSong 10 10
} -returnCodes ok -match orderedLists -result {
    "Ten green bottles hanging on the wall,"
    "Ten green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be nine green bottles hanging on the wall."
    ""
    "Nine green bottles hanging on the wall,"
    "Nine green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be eight green bottles hanging on the wall."
    ""
    "Eight green bottles hanging on the wall,"
    "Eight green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be seven green bottles hanging on the wall."
    ""
    "Seven green bottles hanging on the wall,"
    "Seven green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be six green bottles hanging on the wall."
    ""
    "Six green bottles hanging on the wall,"
    "Six green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be five green bottles hanging on the wall."
    ""
    "Five green bottles hanging on the wall,"
    "Five green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be four green bottles hanging on the wall."
    ""
    "Four green bottles hanging on the wall,"
    "Four green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be three green bottles hanging on the wall."
    ""
    "Three green bottles hanging on the wall,"
    "Three green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be two green bottles hanging on the wall."
    ""
    "Two green bottles hanging on the wall,"
    "Two green bottles hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be one green bottle hanging on the wall."
    ""
    "One green bottle hanging on the wall,"
    "One green bottle hanging on the wall,"
    "And if one green bottle should accidentally fall,"
    "There'll be no green bottles hanging on the wall."
}


cleanupTests
