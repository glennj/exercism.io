#!/usr/bin/env tclsh
# generated: 2026-07-24T01:01:16Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "saddle-points.tcl"


test saddle-points-1 "Can identify single saddle point" -body {
    saddlePoints {
        {9 8 7}
        {5 3 2}
        {6 6 7}
    }
} -returnCodes ok -match unorderedLists -result {{2 1}}

skip saddle-points-2
test saddle-points-2 "Can identify that empty matrix has no saddle points" -body {
    saddlePoints {
        {}
    }
} -returnCodes ok -match unorderedLists -result {}

skip saddle-points-3
test saddle-points-3 "Can identify lack of saddle points when there are none" -body {
    saddlePoints {
        {1 2 3}
        {3 1 2}
        {2 3 1}
    }
} -returnCodes ok -match unorderedLists -result {}

skip saddle-points-4
test saddle-points-4 "Can identify multiple saddle points in a column" -body {
    saddlePoints {
        {4 5 4}
        {3 5 5}
        {1 5 4}
    }
} -returnCodes ok -match unorderedLists -result {{1 2} {2 2} {3 2}}

skip saddle-points-5
test saddle-points-5 "Can identify multiple saddle points in a row" -body {
    saddlePoints {
        {6 7 8}
        {5 5 5}
        {7 5 6}
    }
} -returnCodes ok -match unorderedLists -result {{2 1} {2 2} {2 3}}

skip saddle-points-6
test saddle-points-6 "Can identify saddle point in bottom right corner" -body {
    saddlePoints {
        {8 7 9}
        {6 7 6}
        {3 2 5}
    }
} -returnCodes ok -match unorderedLists -result {{3 3}}

skip saddle-points-7
test saddle-points-7 "Can identify saddle points in a non square matrix" -body {
    saddlePoints {
        {3 1 3}
        {3 2 4}
    }
} -returnCodes ok -match unorderedLists -result {{1 3} {1 1}}

skip saddle-points-8
test saddle-points-8 "Can identify that saddle points in a single column matrix are those with the minimum value" -body {
    saddlePoints {
        {2}
        {1}
        {4}
        {1}
    }
} -returnCodes ok -match unorderedLists -result {{2 1} {4 1}}

skip saddle-points-9
test saddle-points-9 "Can identify that saddle points in a single row matrix are those with the maximum value" -body {
    saddlePoints {
        {2 5 3 5}
    }
} -returnCodes ok -match unorderedLists -result {{1 2} {1 4}}


cleanupTests
