#!/usr/bin/env tclsh
package require tcltest
namespace import ::tcltest::*
source "two-bucket.tcl"

proc fail_fast {} {
    return [expr {
        ![info exists ::env(RUN_ALL)]
        || [string is boolean -strict $::env(RUN_ALL)]
        && !$::env(RUN_ALL)
    }]
}

proc failed {} {
    return [expr {$::tcltest::numTests(Failed) > 0}]
}

if {[fail_fast]} {
    proc test args {
        if {[failed]} {::tcltest::configure -skip *}
        uplevel [list ::tcltest::test {*}$args]
    }
}

proc cleanupTests {} {
    set failed [failed]
    uplevel 1 ::tcltest::cleanupTests
    if {$failed} {exit 1}
}

# compare two dictionaries for the same keys and same values
proc dictionaryMatch {expected actual} {
    if {[dict size $expected] != [dict size $actual]} {
        return false
    }
    dict for {word count} $expected {
        if {![dict exists $actual $word]} {
            return false
        }
        if {[dict get $actual $word] != $count} {
            return false
        }
    }
    return true
}
customMatch dictionary dictionaryMatch

if {$::argv0 eq [info script]} {

    test two-bucket-1 "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one" -body {
        twoBucket {bucketOne 3 bucketTwo 5 goal 1 startBucket one}
    } -result {moves 4 goalBucket one otherBucket 5} -match dictionary -returnCodes ok

    test two-bucket-2 "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two" \
        -body {
            twoBucket {bucketOne 3 bucketTwo 5 goal 1 startBucket two}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 8 goalBucket two otherBucket 3}

    test two-bucket-3 "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one" \
        -body {
            twoBucket {bucketOne 7 bucketTwo 11 goal 2 startBucket one}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 14 goalBucket one otherBucket 11}

    test two-bucket-4 "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two" \
        -body {
            twoBucket {bucketOne 7 bucketTwo 11 goal 2 startBucket two}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 18 goalBucket two otherBucket 7}

    test two-bucket-5 "Measure one step using bucket one of size 2 and bucket two of size 3 - start with bucket two" \
        -body {
            twoBucket {bucketOne 2 bucketTwo 3 goal 3 startBucket two}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 1 goalBucket two otherBucket 0}

    test two-bucket-6 "Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two" \
        -body {
            twoBucket {bucketOne 2 bucketTwo 3 goal 3 startBucket one}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 2 goalBucket two otherBucket 2}

    test two-bucket-7 "Not possible to reach the goal" \
        -body {
            twoBucket {bucketOne 6 bucketTwo 15 goal 5 startBucket one}
        } \
        -returnCodes error \
        -match glob \
        -result "*impossible*"

    test two-bucket-8 "With the same buckets but a different goal, then it is possible" \
        -body {
            twoBucket {bucketOne 6 bucketTwo 15 goal 9 startBucket one}
        } \
        -returnCodes ok \
        -match dictionary \
        -result {moves 10 goalBucket two otherBucket 0}

    test two-bucket-9 "Goal larger than both buckets is impossible" \
        -body {
            twoBucket {bucketOne 5 bucketTwo 7 goal 8 startBucket one}
        } \
        -returnCodes error \
        -match glob \
        -result "*impossible*"

    cleanupTests
}
