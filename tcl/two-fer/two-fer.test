#!/usr/bin/env tclsh
set version 1.2.0
package require tcltest
namespace import ::tcltest::*
source "two-fer.tcl"

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

if {$::argv0 eq [info script]} {

    test two-fer-1 {
        no name given
    } -body {
        two-fer
    } -result "One for you, one for me."

    test two-fer-2 {
        a name given
    } -body {
        two-fer "Alice"
    } -result "One for Alice, one for me."

    test two-fer-3 {
        another name given
    } -body {
        two-fer "Bob"
    } -result "One for Bob, one for me."

    cleanupTests
}
