#!/usr/bin/env tclsh
# generated: 2026-07-17T19:20:41Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "dnd-character.tcl"


test dnd-character-1 "ability modifier for score 3 is -4" -body {
    dnd modifier 3
} -returnCodes ok -result -4

skip dnd-character-2
test dnd-character-2 "ability modifier for score 4 is -3" -body {
    dnd modifier 4
} -returnCodes ok -result -3

skip dnd-character-3
test dnd-character-3 "ability modifier for score 5 is -3" -body {
    dnd modifier 5
} -returnCodes ok -result -3

skip dnd-character-4
test dnd-character-4 "ability modifier for score 6 is -2" -body {
    dnd modifier 6
} -returnCodes ok -result -2

skip dnd-character-5
test dnd-character-5 "ability modifier for score 7 is -2" -body {
    dnd modifier 7
} -returnCodes ok -result -2

skip dnd-character-6
test dnd-character-6 "ability modifier for score 8 is -1" -body {
    dnd modifier 8
} -returnCodes ok -result -1

skip dnd-character-7
test dnd-character-7 "ability modifier for score 9 is -1" -body {
    dnd modifier 9
} -returnCodes ok -result -1

skip dnd-character-8
test dnd-character-8 "ability modifier for score 10 is 0" -body {
    dnd modifier 10
} -returnCodes ok -result 0

skip dnd-character-9
test dnd-character-9 "ability modifier for score 11 is 0" -body {
    dnd modifier 11
} -returnCodes ok -result 0

skip dnd-character-10
test dnd-character-10 "ability modifier for score 12 is +1" -body {
    dnd modifier 12
} -returnCodes ok -result 1

skip dnd-character-11
test dnd-character-11 "ability modifier for score 13 is +1" -body {
    dnd modifier 13
} -returnCodes ok -result 1

skip dnd-character-12
test dnd-character-12 "ability modifier for score 14 is +2" -body {
    dnd modifier 14
} -returnCodes ok -result 2

skip dnd-character-13
test dnd-character-13 "ability modifier for score 15 is +2" -body {
    dnd modifier 15
} -returnCodes ok -result 2

skip dnd-character-14
test dnd-character-14 "ability modifier for score 16 is +3" -body {
    dnd modifier 16
} -returnCodes ok -result 3

skip dnd-character-15
test dnd-character-15 "ability modifier for score 17 is +3" -body {
    dnd modifier 17
} -returnCodes ok -result 3

skip dnd-character-16
test dnd-character-16 "ability modifier for score 18 is +4" -body {
    dnd modifier 18
} -returnCodes ok -result 4





skip dnd-character-17
test dnd-character-17 "random ability is within range" -body {
    set result true
    for {set i 0} {$i < 10000} {incr i} {
        set a [dnd ability]
        if {$a < 3 || $a > 18} {
            set result false
            break
        }
    }
    set result
} -returnCodes ok -match boolean -result true

skip dnd-character-18
test dnd-character-18 "a character is a well-formed dict" -body {
    set character [dnd character]
    expr {[string is list -strict $character] && [llength $character] % 2 == 0}
} -returnCodes ok -match boolean -result true

skip dnd-character-19
test dnd-character-19 "a character has the correct attributes" -body {
    set character [dnd character]
    lsort [dict keys $character]
} -returnCodes ok -match unorderedLists -result {charisma constitution dexterity hitpoints intelligence strength wisdom}

skip dnd-character-20
test dnd-character-20 "characteristics have the correct values" -body {
    set character [dnd character]
    dict with character {
        expr {
            3 <= $charisma && $charisma <= 18 &&
            3 <= $constitution && $constitution <= 18 &&
            3 <= $dexterity && $dexterity <= 18 &&
            3 <= $intelligence && $intelligence <= 18 &&
            3 <= $strength && $strength <= 18 &&
            3 <= $wisdom && $wisdom <= 18 &&
            $hitpoints == 10 + [dnd modifier $constitution]
        }
    }
} -returnCodes ok -match boolean -result true

cleanupTests
