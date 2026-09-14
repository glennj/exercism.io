#!/usr/bin/env tclsh
# generated: 2026-07-24T17:41:34Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "split-second-stopwatch.tcl"


############################################################
# Mock the builtin `clock` command to be able to simulate
# the passage of time.

rename clock tcl_clock
namespace eval clock {
    variable epoch 0

    set clock_ensemble [namespace ensemble configure ::tcl_clock -map]

    dict set clock_ensemble advance [namespace current]::advance
    dict set clock_ensemble seconds [namespace current]::seconds

    namespace ensemble create -map $clock_ensemble

    proc advance {timestamp} {
        scan $timestamp {%d:%d:%d} h m s
        set duration [expr {$h * 3600 + $m * 60 + $s}]

        variable epoch
        incr epoch $duration
    }

    proc seconds {} {
        variable epoch
        return $epoch
    }
}
############################################################

test split-second-stopwatch-1 "new stopwatch starts in ready state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch state
} -returnCodes ok -result "ready"

skip split-second-stopwatch-2
test split-second-stopwatch-2 "new stopwatch's current lap has no elapsed time" -body {
    set stopwatch [Stopwatch new]
    $stopwatch currentLap
} -returnCodes ok -result {00:00:00}

skip split-second-stopwatch-3
test split-second-stopwatch-3 "new stopwatch's total has no elapsed time" -body {
    set stopwatch [Stopwatch new]
    $stopwatch total
} -returnCodes ok -result {00:00:00}

skip split-second-stopwatch-4
test split-second-stopwatch-4 "new stopwatch does not have previous laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch previousLaps
} -returnCodes ok -result {}

skip split-second-stopwatch-5
test split-second-stopwatch-5 "start from ready state changes state to running" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch state
} -returnCodes ok -result "running"

skip split-second-stopwatch-6
test split-second-stopwatch-6 "start does not change previous laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch previousLaps
} -returnCodes ok -result {}

skip split-second-stopwatch-7
test split-second-stopwatch-7 "start initiates time tracking for current lap" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:05"
    $stopwatch currentLap
} -returnCodes ok -result {00:00:05}

skip split-second-stopwatch-8
test split-second-stopwatch-8 "start initiates time tracking for total" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:23"
    $stopwatch total
} -returnCodes ok -result {00:00:23}

skip split-second-stopwatch-9
test split-second-stopwatch-9 "start cannot be called from running state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch start
} -returnCodes error -result "cannot start an already running stopwatch"

skip split-second-stopwatch-10
test split-second-stopwatch-10 "stop from running state changes state to stopped" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch stop
    $stopwatch state
} -returnCodes ok -result "stopped"

skip split-second-stopwatch-11
test split-second-stopwatch-11 "stop pauses time tracking for current lap" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:05"
    $stopwatch stop
    clock advance "00:00:08"
    $stopwatch currentLap
} -returnCodes ok -result {00:00:05}

skip split-second-stopwatch-12
test split-second-stopwatch-12 "stop pauses time tracking for total" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:13"
    $stopwatch stop
    clock advance "00:00:44"
    $stopwatch total
} -returnCodes ok -result {00:00:13}

skip split-second-stopwatch-13
test split-second-stopwatch-13 "stop cannot be called from ready state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch stop
} -returnCodes error -result "cannot stop a stopwatch that is not running"

skip split-second-stopwatch-14
test split-second-stopwatch-14 "stop cannot be called from stopped state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch stop
    $stopwatch stop
} -returnCodes error -result "cannot stop a stopwatch that is not running"

skip split-second-stopwatch-15
test split-second-stopwatch-15 "start from stopped state changes state to running" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch stop
    $stopwatch start
    $stopwatch state
} -returnCodes ok -result "running"

skip split-second-stopwatch-16
test split-second-stopwatch-16 "start from stopped state resumes time tracking for current lap" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:01:20"
    $stopwatch stop
    clock advance "00:00:20"
    $stopwatch start
    clock advance "00:00:08"
    $stopwatch currentLap
} -returnCodes ok -result {00:01:28}

skip split-second-stopwatch-17
test split-second-stopwatch-17 "start from stopped state resumes time tracking for total" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:23"
    $stopwatch stop
    clock advance "00:00:44"
    $stopwatch start
    clock advance "00:00:09"
    $stopwatch total
} -returnCodes ok -result {00:00:32}

skip split-second-stopwatch-18
test split-second-stopwatch-18 "lap adds current lap to previous laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:01:38"
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    clock advance "00:00:44"
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    set saved
} -setup {
    set saved {}
} -returnCodes ok -match dictionary -result {
    previous {00:01:38 {00:01:38 00:00:44}}
}

skip split-second-stopwatch-19
test split-second-stopwatch-19 "lap resets current lap and resumes time tracking" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:08:22"
    $stopwatch lap
    dict lappend saved current [$stopwatch currentLap]
    clock advance "00:00:15"
    dict lappend saved current [$stopwatch currentLap]
    set saved
} -setup {
    set saved {}
} -returnCodes ok -match dictionary -result {
    current {00:00:00 00:00:15}
}

skip split-second-stopwatch-20
test split-second-stopwatch-20 "lap continues time tracking for total" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:22"
    $stopwatch lap
    clock advance "00:00:33"
    $stopwatch total
} -returnCodes ok -result {00:00:55}

skip split-second-stopwatch-21
test split-second-stopwatch-21 "lap cannot be called from ready state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch lap
} -returnCodes error -result "cannot lap a stopwatch that is not running"

skip split-second-stopwatch-22
test split-second-stopwatch-22 "lap cannot be called from stopped state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch stop
    $stopwatch lap
} -returnCodes error -result "cannot lap a stopwatch that is not running"

skip split-second-stopwatch-23
test split-second-stopwatch-23 "stop does not change previous laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:11:22"
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    $stopwatch stop
    dict lappend saved previous [$stopwatch previousLaps]
    set saved
} -setup {
    set saved {}
} -returnCodes ok -match dictionary -result {
    previous {00:11:22 00:11:22}
}

skip split-second-stopwatch-24
test split-second-stopwatch-24 "reset from stopped state changes state to ready" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch stop
    $stopwatch reset
    $stopwatch state
} -returnCodes ok -result "ready"

skip split-second-stopwatch-25
test split-second-stopwatch-25 "reset resets current lap" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:10"
    $stopwatch stop
    $stopwatch reset
    $stopwatch currentLap
} -returnCodes ok -result {00:00:00}

skip split-second-stopwatch-26
test split-second-stopwatch-26 "reset clears previous laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "00:00:10"
    $stopwatch lap
    clock advance "00:00:20"
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    $stopwatch stop
    $stopwatch reset
    dict lappend saved previous [$stopwatch previousLaps]
    set saved
} -setup {
    set saved {}
} -returnCodes ok -match dictionary -result {
    previous {{00:00:10 00:00:20} {}}
}

skip split-second-stopwatch-27
test split-second-stopwatch-27 "reset cannot be called from ready state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch reset
} -returnCodes error -result "cannot reset a stopwatch that is not stopped"

skip split-second-stopwatch-28
test split-second-stopwatch-28 "reset cannot be called from running state" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    $stopwatch reset
} -returnCodes error -result "cannot reset a stopwatch that is not stopped"

skip split-second-stopwatch-29
test split-second-stopwatch-29 "supports very long laps" -body {
    set stopwatch [Stopwatch new]
    $stopwatch start
    clock advance "01:23:45"
    dict lappend saved current [$stopwatch currentLap]
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    clock advance "04:01:40"
    dict lappend saved current [$stopwatch currentLap]
    dict lappend saved total [$stopwatch total]
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    clock advance "08:43:05"
    dict lappend saved current [$stopwatch currentLap]
    dict lappend saved total [$stopwatch total]
    $stopwatch lap
    dict lappend saved previous [$stopwatch previousLaps]
    set saved
} -setup {
    set saved {}
} -returnCodes ok -match dictionary -result {
    current {01:23:45 04:01:40 08:43:05}
    previous {01:23:45 {01:23:45 04:01:40} {01:23:45 04:01:40 08:43:05}}
    total {05:25:25 14:08:30}
}


cleanupTests
