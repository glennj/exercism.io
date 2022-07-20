#!tclsh

# global vars
set badGuesses 9
set initialState ongoing
set state $initialState
set charMap {
    A _ B _ C _ D _ E _ F _ G _ H _ I _ J _ K _ L _ M _
    N _ O _ P _ Q _ R _ S _ T _ U _ V _ W _ X _ Y _ Z _
}

proc startServer {args} {
    set ::word [string toupper [lindex $args 0]]

    # use port 0 to allow Tcl to find an unused port
    set s [socket -server incoming 0]

    # communicate the port number in use
    set sockInfo [chan configure $s -sockname]
    set ::env(HANGMAN_PORT) [lindex $sockInfo 2]
    log "server started on port $::env(HANGMAN_PORT) with $::word"

    # enter the event loop
    vwait forever
    log "exiting"
}


proc timestamp {} {
    clock format [clock seconds] -format "%Y-%m-%d %T"
}

proc log {msg} {
    puts "[timestamp] - $msg"
}

proc incoming {sock addr port} {
    log "incoming connection from $addr:$port"
    dict set ::clients $sock "$addr:$port"
    chan configure $sock -buffering line
    chan event $sock readable [list hangman $sock]
}

proc hangman {sock} {
    global word
    set client [dict get $::clients $sock]

    if {[eof $sock]} {
        log "session ended with $client"
        dict unset clients $sock
        close $sock
        return
    }

    if {![gets $sock line]} {
        log "can't read a line from $client"
        return
    }

    set words [regexp -all -inline {\w+} [string toupper $line]]

    set words [lassign $words cmd]
    log "from $client got $cmd with args [list $words]"

    switch -exact -- $cmd {
        SHUTDOWN {
            log "shutting down"
            set ::forever now
        }
        STATUS {
            puts $sock [currentStatus]
        }
        GUESS {
            set letter [string index [lindex $words 0] 0]
            guess $letter
            puts $sock [currentStatus]
            if {[gameOver]} {
                set ::forever now
            }
        }
        default {
            puts $sock [list ERROR "unknown command $cmd"]
        }
    }
}

proc gameOver {} {
    expr {$::state ne $::initialState}
}

proc currentStatus {} {
    return [list $::badGuesses [mask] $::state]
}

proc mask {} {
    string map $::charMap $::word
}

proc guess {letter} {
    global badGuesses state charMap word

    if {   ![dict exists $charMap $letter]
        || [dict get $charMap $letter] eq $letter
        || [string first $letter $word] == -1
    } {
        # not a letter or already guessed it or letter not in word
        incr badGuesses -1
    } else {
        # OK, register the guess
        dict set charMap $letter $letter
    }

    if {$badGuesses == 0} {
        set state lose
    } elseif {[mask] eq $word} {
        set state win
    }
}

startServer {*}$argv
