#!/usr/bin/env tclsh
package require tcltest
namespace import ::tcltest::*
package require Thread
source testHelpers.tcl

############################################################
proc launchGame {word} {
    global serverThread env

    # Start the server in a separate thread:
    # To avoid port collisions, we don't hardcode a port number here.
    # Make sure your code sets `env(HANGMAN_PORT)` with the port
    # your server is running on!
    set serverThread [thread::create]
    thread::send $serverThread [list set argv $word]
    thread::send -async $serverThread "source hangman.tcl"
    after 100
    if {![info exists env(HANGMAN_PORT)]} {
        error "Can't find the port for the Hangman server!"
    }

    # and initiate the client connection
    global sock
    set sock [socket localhost $env(HANGMAN_PORT)]
    fconfigure $sock -buffering line
}

proc quitGame {} {
    global sock serverThread
    puts $sock "shutdown"
    close $sock
    thread::release $serverThread
}

############################################################
test hangman-1 "initial state" -setup {
    launchGame "apple"
} -body {
    global sock
    puts $sock "status"
    gets $sock initialState
    set initialState
} -cleanup quitGame -result "9 _____ ongoing"

skip hangman-2
test hangman-2 "after 10 failures the game is over" -setup {
    launchGame "cat"
} -body {
    global sock
    for {set i 1} {$i < 10} {incr i} {
        puts $sock "guess x"
        gets $sock state
    }
    set state
} -cleanup quitGame -result "0 ___ lose"

skip hangman-3
test hangman-3 "a correct letter removes underscores" -setup {
    launchGame "banana"
} -body {
    global sock
    set states {}
    puts $sock "Guess a"
    lappend states [gets $sock]
    puts $sock "GUESS b"
    lappend states [gets $sock]
    set states
} -cleanup quitGame -match orderedLists -result {
    "9 _A_A_A ongoing"
    "9 BA_A_A ongoing"
}

skip hangman-4
test hangman-4 "a correct letter a second time is a failure" -setup {
    launchGame "noodle"
} -body {
    global sock
    set states {}
    puts $sock "guess o"
    lappend states [gets $sock]
    puts $sock "guess o"
    lappend states [gets $sock]
    set states
} -cleanup quitGame -match orderedLists -result {
    "9 _OO___ ongoing"
    "8 _OO___ ongoing"
}

skip hangman-5
test hangman-5 "guessing all the letters" -setup {
    launchGame "noodle"
} -body {
    global sock
    set states {}
    puts $sock "guess o"
    lappend states [gets $sock]
    puts $sock "guess e"
    lappend states [gets $sock]
    puts $sock "guess d"
    lappend states [gets $sock]
    puts $sock "guess n"
    lappend states [gets $sock]
    puts $sock "guess x"
    lappend states [gets $sock]
    puts $sock "guess l"
    lappend states [gets $sock]
    set states
} -cleanup quitGame -match orderedLists -result {
    "9 _OO___ ongoing"
    "9 _OO__E ongoing"
    "9 _OOD_E ongoing"
    "9 NOOD_E ongoing"
    "8 NOOD_E ongoing"
    "8 NOODLE win"
}

skip hangman-6
test hangman-6 "winning on the last guess is still a win" -setup {
    launchGame iiiii
} -body {
    global sock
    puts $sock "guess a"
    gets $sock state
    puts $sock "guess b"
    gets $sock state
    puts $sock "guess c"
    gets $sock state
    puts $sock "guess d"
    gets $sock state
    puts $sock "guess e"
    gets $sock state
    puts $sock "guess f"
    gets $sock state
    puts $sock "guess g"
    gets $sock state
    puts $sock "guess h"
    gets $sock state
    puts $sock "guess i"
    gets $sock state
    set state
} -cleanup quitGame -result "1 IIIII win"


cleanupTests
