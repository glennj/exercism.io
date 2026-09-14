#!/usr/bin/env tclsh
# generated: 2026-07-26T12:44:22Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "high-scores.tcl"


test high-scores-1 "List of scores" -body {
    set hs [HighScores new]
    $hs addScores 30 50 20 70
    $hs scores
} -returnCodes ok -result {30 50 20 70}

skip high-scores-2
test high-scores-2 "Latest score" -body {
    set hs [HighScores new]
    $hs addScores 100 0 90 30
    $hs latest
} -returnCodes ok -result 30

skip high-scores-3
test high-scores-3 "Personal best" -body {
    set hs [HighScores new]
    $hs addScores 40 100 70
    $hs personalBest
} -returnCodes ok -result 100

skip high-scores-4
test high-scores-4 "Personal top three from a list of scores" -body {
    set hs [HighScores new]
    $hs addScores 10 30 90 30 100 20 10 0 30 40 40 70 70
    $hs topThree
} -returnCodes ok -result {100 90 70}

skip high-scores-5
test high-scores-5 "Personal top highest to lowest" -body {
    set hs [HighScores new]
    $hs addScores 20 10 30
    $hs topThree
} -returnCodes ok -result {30 20 10}

skip high-scores-6
test high-scores-6 "Personal top when there is a tie" -body {
    set hs [HighScores new]
    $hs addScores 40 20 40 30
    $hs topThree
} -returnCodes ok -result {40 40 30}

skip high-scores-7
test high-scores-7 "Personal top when there are less than 3" -body {
    set hs [HighScores new]
    $hs addScores 30 70
    $hs topThree
} -returnCodes ok -result {70 30}

skip high-scores-8
test high-scores-8 "Personal top when there is only one" -body {
    set hs [HighScores new]
    $hs addScores 40
    $hs topThree
} -returnCodes ok -result {40}

skip high-scores-9
test high-scores-9 "Latest score after personal top scores" -body {
    set hs [HighScores new]
    $hs addScores 70 50 20 30
    set top3 [$hs topThree]
    $hs latest
} -returnCodes ok -result 30

skip high-scores-10
test high-scores-10 "Scores after personal top scores" -body {
    set hs [HighScores new]
    $hs addScores 30 50 20 70
    set top3 [$hs topThree]
    $hs scores
} -returnCodes ok -result {30 50 20 70}

skip high-scores-11
test high-scores-11 "Latest score after personal best" -body {
    set hs [HighScores new]
    $hs addScores 20 70 15 25 30
    set best [$hs personalBest]
    $hs latest
} -returnCodes ok -result 30

skip high-scores-12
test high-scores-12 "Scores after personal best" -body {
    set hs [HighScores new]
    $hs addScores 20 70 15 25 30
    set best [$hs personalBest]
    $hs scores
} -returnCodes ok -result {20 70 15 25 30}


cleanupTests
