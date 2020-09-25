#!/usr/bin/env tclsh
set version 1.1.0
package require tcltest
namespace import ::tcltest::*
source "hello-world.tcl"

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

    test hello-1 {
        Say Hi!
    } -body {
        hello
    } -result "Hello, World!"

    cleanupTests
}
