#!/usr/bin/env tclsh
# generated: 2026-07-17T12:00:09Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "circular-buffer.tcl"


test circular-buffer-0.1 "new buffer is empty" -body {
    set b [CircularBuffer new 2]
    $b empty?
} -returnCodes ok -match boolean -result true

skip circular-buffer-0.2
test circular-buffer-0.2 "non-empty buffer is not empty" -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b empty?
} -returnCodes ok -match boolean -result false

skip circular-buffer-0.3
test circular-buffer-0.3 "full buffer is full" -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b write 2
    $b full?
} -returnCodes ok -match boolean -result true

skip circular-buffer-0.4
test circular-buffer-0.4 "non-full buffer is not full" -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b full?
} -returnCodes ok -match boolean -result false

skip circular-buffer-1
test circular-buffer-1 "reading empty buffer should fail" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b read
} -returnCodes error -result "buffer is empty"

skip circular-buffer-2
test circular-buffer-2 "can read an item just written" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    lappend values [$b read]
    set values
} -returnCodes ok -result {1}

skip circular-buffer-3
test circular-buffer-3 "each item may only be read once" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    lappend values [$b read]
    $b read
} -returnCodes error -result "buffer is empty"

skip circular-buffer-4
test circular-buffer-4 "items are read in the order they are written" -setup {set values {}} -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b write 2
    lappend values [$b read]
    lappend values [$b read]
    set values
} -returnCodes ok -result {1 2}

skip circular-buffer-5
test circular-buffer-5 "full buffer can't be written to" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    $b write 2
} -returnCodes error -result "buffer is full"

skip circular-buffer-6
test circular-buffer-6 "a read frees up capacity for another write" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    lappend values [$b read]
    $b write 2
    lappend values [$b read]
    set values
} -returnCodes ok -result {1 2}

skip circular-buffer-7
test circular-buffer-7 "read position is maintained even across multiple writes" -setup {set values {}} -body {
    set b [CircularBuffer new 3]
    $b write 1
    $b write 2
    lappend values [$b read]
    $b write 3
    lappend values [$b read]
    lappend values [$b read]
    set values
} -returnCodes ok -result {1 2 3}

skip circular-buffer-8
test circular-buffer-8 "items cleared out of buffer can't be read" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    $b clear
    $b read
} -returnCodes error -result "buffer is empty"

skip circular-buffer-9
test circular-buffer-9 "clear frees up capacity for another write" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b write 1
    $b clear
    $b write 2
    lappend values [$b read]
    set values
} -returnCodes ok -result {2}

skip circular-buffer-10
test circular-buffer-10 "clear does nothing on empty buffer" -setup {set values {}} -body {
    set b [CircularBuffer new 1]
    $b clear
    $b write 1
    lappend values [$b read]
    set values
} -returnCodes ok -result {1}

skip circular-buffer-11
test circular-buffer-11 "overwrite acts like write on non-full buffer" -setup {set values {}} -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b overwrite 2
    lappend values [$b read]
    lappend values [$b read]
    set values
} -returnCodes ok -result {1 2}

skip circular-buffer-12
test circular-buffer-12 "overwrite replaces the oldest item on full buffer" -setup {set values {}} -body {
    set b [CircularBuffer new 2]
    $b write 1
    $b write 2
    $b overwrite 3
    lappend values [$b read]
    lappend values [$b read]
    set values
} -returnCodes ok -result {2 3}

skip circular-buffer-13
test circular-buffer-13 "overwrite replaces the oldest item remaining in buffer following a read" -setup {set values {}} -body {
    set b [CircularBuffer new 3]
    $b write 1
    $b write 2
    $b write 3
    lappend values [$b read]
    $b write 4
    $b overwrite 5
    lappend values [$b read]
    lappend values [$b read]
    lappend values [$b read]
    set values
} -returnCodes ok -result {1 3 4 5}

skip circular-buffer-14
test circular-buffer-14 "initial clear does not affect wrapping around" -setup {set values {}} -body {
    set b [CircularBuffer new 2]
    $b clear
    $b write 1
    $b write 2
    $b overwrite 3
    $b overwrite 4
    lappend values [$b read]
    lappend values [$b read]
    # some inelegant code: ensure the 3rd read is the one with the error.
    try {
        $b read
    } on error {} {
        lappend values "err"
    }
    set values
} -returnCodes ok -result {3 4 err}


cleanupTests
