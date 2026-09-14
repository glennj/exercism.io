#!/usr/bin/env tclsh
# generated: 2026-07-16T20:20:13Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "affine-cipher.tcl"


test affine-cipher-1 "encode yes" -body {
    affine_encode "yes" 5 7
} -returnCodes ok -result "xbt"

skip affine-cipher-2
test affine-cipher-2 "encode no" -body {
    affine_encode "no" 15 18
} -returnCodes ok -result "fu"

skip affine-cipher-3
test affine-cipher-3 "encode OMG" -body {
    affine_encode "OMG" 21 3
} -returnCodes ok -result "lvz"

skip affine-cipher-4
test affine-cipher-4 "encode O M G" -body {
    affine_encode "O M G" 25 47
} -returnCodes ok -result "hjp"

skip affine-cipher-5
test affine-cipher-5 "encode mindblowingly" -body {
    affine_encode "mindblowingly" 11 15
} -returnCodes ok -result "rzcwa gnxzc dgt"

skip affine-cipher-6
test affine-cipher-6 "encode numbers" -body {
    affine_encode "Testing,1 2 3, testing." 3 4
} -returnCodes ok -result "jqgjc rw123 jqgjc rw"

skip affine-cipher-7
test affine-cipher-7 "encode deep thought" -body {
    affine_encode "Truth is fiction." 5 17
} -returnCodes ok -result "iynia fdqfb ifje"

skip affine-cipher-8
test affine-cipher-8 "encode all the letters" -body {
    affine_encode "The quick brown fox jumps over the lazy dog." 17 33
} -returnCodes ok -result "swxtj npvyk lruol iejdc blaxk swxmh qzglf"

skip affine-cipher-9
test affine-cipher-9 "encode with a not coprime to m" -body {
    affine_encode "This is a test." 6 17
} -returnCodes error -result "a and m must be coprime."

skip affine-cipher-10
test affine-cipher-10 "decode exercism" -body {
    affine_decode "tytgn fjr" 3 7
} -returnCodes ok -result "exercism"

skip affine-cipher-11
test affine-cipher-11 "decode a sentence" -body {
    affine_decode "qdwju nqcro muwhn odqun oppmd aunwd o" 19 16
} -returnCodes ok -result "anobstacleisoftenasteppingstone"

skip affine-cipher-12
test affine-cipher-12 "decode numbers" -body {
    affine_decode "odpoz ub123 odpoz ub" 25 7
} -returnCodes ok -result "testing123testing"

skip affine-cipher-13
test affine-cipher-13 "decode all the letters" -body {
    affine_decode "swxtj npvyk lruol iejdc blaxk swxmh qzglf" 17 33
} -returnCodes ok -result "thequickbrownfoxjumpsoverthelazydog"

skip affine-cipher-14
test affine-cipher-14 "decode with no spaces in input" -body {
    affine_decode "swxtjnpvyklruoliejdcblaxkswxmhqzglf" 17 33
} -returnCodes ok -result "thequickbrownfoxjumpsoverthelazydog"

skip affine-cipher-15
test affine-cipher-15 "decode with too many spaces" -body {
    affine_decode "vszzm    cly   yd cg    qdp" 15 16
} -returnCodes ok -result "jollygreengiant"

skip affine-cipher-16
test affine-cipher-16 "decode with a not coprime to m" -body {
    affine_decode "Test" 13 5
} -returnCodes error -result "a and m must be coprime."


cleanupTests
