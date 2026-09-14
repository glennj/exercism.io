#!/usr/bin/env tclsh
# generated: 2026-07-23T01:22:41Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "pythagorean-triplet.tcl"


test pythagorean-triplet-1 "triplets whose sum is 12" -body {
    tripletsWithSum 12
} -returnCodes ok -match unorderedLists -result {
    {3 4 5}
}

skip pythagorean-triplet-2
test pythagorean-triplet-2 "triplets whose sum is 108" -body {
    tripletsWithSum 108
} -returnCodes ok -match unorderedLists -result {
    {27 36 45}
}

skip pythagorean-triplet-3
test pythagorean-triplet-3 "triplets whose sum is 1000" -body {
    tripletsWithSum 1000
} -returnCodes ok -match unorderedLists -result {
    {200 375 425}
}

skip pythagorean-triplet-4
test pythagorean-triplet-4 "no matching triplets for 1001" -body {
    tripletsWithSum 1001
} -returnCodes ok -match unorderedLists -result {}

skip pythagorean-triplet-5
test pythagorean-triplet-5 "returns all matching triplets" -body {
    tripletsWithSum 90
} -returnCodes ok -match unorderedLists -result {
    {9 40 41}
    {15 36 39}
}

skip pythagorean-triplet-6
test pythagorean-triplet-6 "several matching triplets" -body {
    tripletsWithSum 840
} -returnCodes ok -match unorderedLists -result {
    {40 399 401}
    {56 390 394}
    {105 360 375}
    {120 350 370}
    {140 336 364}
    {168 315 357}
    {210 280 350}
    {240 252 348}
}

skip pythagorean-triplet-7
test pythagorean-triplet-7 "triplets for large number" -body {
    tripletsWithSum 30000
} -returnCodes ok -match unorderedLists -result {
    {1200 14375 14425}
    {1875 14000 14125}
    {5000 12000 13000}
    {6000 11250 12750}
    {7500 10000 12500}
}


cleanupTests
