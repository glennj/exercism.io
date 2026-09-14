#!/usr/bin/env tclsh
# generated: 2026-07-23T00:52:06Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "pov.tcl"


# "trees" are shown as dict literals here
# each node in a tree has:
#    "label" - a string
#    "children" - (optional) a list of child nodes)

test pov-1 "Reroot a tree so that its root is the specified node.: Results in the same tree if the input tree is a singleton" -body {
    set input {label x}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x}

skip pov-2
test pov-2 "Reroot a tree so that its root is the specified node.: Can reroot a tree with a parent and one sibling" -body {
    set input {label parent children {{label x} {label sibling}}}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x children {{label parent children {{label sibling}}}}}

skip pov-3
test pov-3 "Reroot a tree so that its root is the specified node.: Can reroot a tree with a parent and many siblings" -body {
    set input {label parent children {{label a} {label x} {label b} {label c}}}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x children {{label parent children {{label a} {label b} {label c}}}}}

skip pov-4
test pov-4 "Reroot a tree so that its root is the specified node.: Can reroot a tree with new root deeply nested in tree" -body {
    set input {label level-0 children {{label level-1 children {{label level-2 children {{label level-3 children {{label x}}}}}}}}}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x children {{label level-3 children {{label level-2 children {{label level-1 children {{label level-0}}}}}}}}}

skip pov-5
test pov-5 "Reroot a tree so that its root is the specified node.: Moves children of the new root to same level as former parent" -body {
    set input {label parent children {{label x children {{label kid-0} {label kid-1}}}}}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x children {{label kid-0} {label kid-1} {label parent}}}

skip pov-6
test pov-6 "Reroot a tree so that its root is the specified node.: Can reroot a complex tree with cousins" -body {
    set input {label grandparent children {{label parent children {{label x children {{label kid-0} {label kid-1}}} {label sibling-0} {label sibling-1}}} {label uncle children {{label cousin-0} {label cousin-1}}}}}
    fromPov $input x
} -returnCodes ok -match dictionary -result {label x children {{label kid-0} {label kid-1} {label parent children {{label sibling-0} {label sibling-1} {label grandparent children {{label uncle children {{label cousin-0} {label cousin-1}}}}}}}}}

skip pov-7
test pov-7 "Reroot a tree so that its root is the specified node.: Errors if target does not exist in a singleton tree" -body {
    set input {label x}
    fromPov $input nonexistent
} -returnCodes error -result "no such target"

skip pov-8
test pov-8 "Reroot a tree so that its root is the specified node.: Errors if target does not exist in a large tree" -body {
    set input {label parent children {{label x children {{label kid-0} {label kid-1}}} {label sibling-0} {label sibling-1}}}
    fromPov $input nonexistent
} -returnCodes error -result "no such target"

skip pov-9
test pov-9 "Given two nodes, find the path between them: Can find path to parent" -body {
    set input {label parent children {{label x} {label sibling}}}
    path $input x parent
} -returnCodes ok -match orderedLists -result {x parent}

skip pov-10
test pov-10 "Given two nodes, find the path between them: Can find path to sibling" -body {
    set input {label parent children {{label a} {label x} {label b} {label c}}}
    path $input x b
} -returnCodes ok -match orderedLists -result {x parent b}

skip pov-11
test pov-11 "Given two nodes, find the path between them: Can find path to cousin" -body {
    set input {label grandparent children {{label parent children {{label x children {{label kid-0} {label kid-1}}} {label sibling-0} {label sibling-1}}} {label uncle children {{label cousin-0} {label cousin-1}}}}}
    path $input x cousin-1
} -returnCodes ok -match orderedLists -result {x parent grandparent uncle cousin-1}

skip pov-12
test pov-12 "Given two nodes, find the path between them: Can find path not involving root" -body {
    set input {label grandparent children {{label parent children {{label x} {label sibling-0} {label sibling-1}}}}}
    path $input x sibling-1
} -returnCodes ok -match orderedLists -result {x parent sibling-1}

skip pov-13
test pov-13 "Given two nodes, find the path between them: Can find path from nodes other than x" -body {
    set input {label parent children {{label a} {label x} {label b} {label c}}}
    path $input a c
} -returnCodes ok -match orderedLists -result {a parent c}

skip pov-14
test pov-14 "Given two nodes, find the path between them: Errors if destination does not exist" -body {
    set input {label parent children {{label x children {{label kid-0} {label kid-1}}} {label sibling-0} {label sibling-1}}}
    path $input x nonexistent
} -returnCodes error -result "no such label"

skip pov-15
test pov-15 "Given two nodes, find the path between them: Errors if source does not exist" -body {
    set input {label parent children {{label x children {{label kid-0} {label kid-1}}} {label sibling-0} {label sibling-1}}}
    path $input nonexistent x
} -returnCodes error -result "no such label"


cleanupTests
