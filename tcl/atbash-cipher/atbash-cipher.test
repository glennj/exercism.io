#!/usr/bin/env tclsh
# generated: 2026-07-16T22:05:02Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "atbash-cipher.tcl"


test atbash-cipher-1 "encode yes" -body {
    atbash encode "yes"
} -returnCodes ok -result "bvh"

skip atbash-cipher-2
test atbash-cipher-2 "encode no" -body {
    atbash encode "no"
} -returnCodes ok -result "ml"

skip atbash-cipher-3
test atbash-cipher-3 "encode OMG" -body {
    atbash encode "OMG"
} -returnCodes ok -result "lnt"

skip atbash-cipher-4
test atbash-cipher-4 "encode spaces" -body {
    atbash encode "O M G"
} -returnCodes ok -result "lnt"

skip atbash-cipher-5
test atbash-cipher-5 "encode mindblowingly" -body {
    atbash encode "mindblowingly"
} -returnCodes ok -result "nrmwy oldrm tob"

skip atbash-cipher-6
test atbash-cipher-6 "encode numbers" -body {
    atbash encode "Testing,1 2 3, testing."
} -returnCodes ok -result "gvhgr mt123 gvhgr mt"

skip atbash-cipher-7
test atbash-cipher-7 "encode deep thought" -body {
    atbash encode "Truth is fiction."
} -returnCodes ok -result "gifgs rhurx grlm"

skip atbash-cipher-8
test atbash-cipher-8 "encode all the letters" -body {
    atbash encode "The quick brown fox jumps over the lazy dog."
} -returnCodes ok -result "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"

skip atbash-cipher-9
test atbash-cipher-9 "decode exercism" -body {
    atbash decode "vcvix rhn"
} -returnCodes ok -result "exercism"

skip atbash-cipher-10
test atbash-cipher-10 "decode a sentence" -body {
    atbash decode "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
} -returnCodes ok -result "anobstacleisoftenasteppingstone"

skip atbash-cipher-11
test atbash-cipher-11 "decode numbers" -body {
    atbash decode "gvhgr mt123 gvhgr mt"
} -returnCodes ok -result "testing123testing"

skip atbash-cipher-12
test atbash-cipher-12 "decode all the letters" -body {
    atbash decode "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
} -returnCodes ok -result "thequickbrownfoxjumpsoverthelazydog"

skip atbash-cipher-13
test atbash-cipher-13 "decode with too many spaces" -body {
    atbash decode "vc vix    r hn"
} -returnCodes ok -result "exercism"

skip atbash-cipher-14
test atbash-cipher-14 "decode with no spaces" -body {
    atbash decode "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
} -returnCodes ok -result "anobstacleisoftenasteppingstone"


cleanupTests
