proc abbreviate {phrase} {
    # Replace all non-alphabetic non-apostrophe characters with space.
    set phrase [regsub -all {[^[:alpha:]']} $phrase " "]

    # Find the first character of each word.  Note that `split` splits on
    # individual spaces, not sequences of whitespace: some of the words will
    # be zero-length.  But the first character of a zero-length string is
    # the empty string.
    set acronym ""
    foreach word [split $phrase] {
        append acronym [string range $word 0 0]
    }

    return [string toupper $acronym]
}

# Add a few extra implementations, and some benchmarking


# instead of replacing unwanted chars with spaces and then splitting
# the result, use a regex to extract the wanted words
proc abbreviate_findwords {phrase} {
    set regex {[[:alpha:]][\w']*}
    foreach word [regexp -all -inline $regex $phrase] {
        append acronym [string index $word 0]
    }
    string toupper $acronym
}

# refine the above to extract each word's initial
proc abbreviate_findinitials {phrase} {
    set regex {([[:alpha:]])[\w']*}
    foreach {word init} [regexp -all -inline $regex $phrase] {
        append acronym $init
    }
    string toupper $acronym
}

proc abbreviate_splitchars {phrase} {
    set state A
    set acronym ""
    foreach char [split $phrase ""] {
        switch -exact $state {
            A { # seeking the next alpha to add to the acronym
                if {[string is alpha -strict $char]} {
                    append acronym $char
                    set state N
                }
            }
            N { # seeking the next NON-alpha
                if {!($char eq {'} || [string is alpha -strict $char])} {
                    set state A
                }
            }
        }
    }
    string toupper $acronym
}

proc abbreviate_bychar {phrase} {
    set state A
    set acronym ""
    set len [string length $phrase]
    for {set i 0} {$i < $len} {incr i} {
        set char [string index $phrase $i]
        switch -exact $state {
            A { # seeking the next alpha to add to the acronym
                if {[string is alpha -strict $char]} {
                    append acronym $char
                    set state N
                }
            }
            N { # seeking the next NON-alpha
                if {!($char eq {'} || [string is alpha -strict $char])} {
                    set state A
                }
            }
        }
    }
    string toupper $acronym
}

############################################################
proc benchmark {} {
    set procs [info procs abbreviate*]

    set text "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
    set acronym "SUPUOINESVADLTRAEIQAIIVEQABVDSENEIVQVSAAOAFSQCMDEQRVSNNPQEQDIQDSACAVSQNNEMTIULEDMAQVUEAMVQNEUCSLNUAEECCQAVEIRQIEVVEQNMCVIQDEFQVNP"

    # validate the procs work
    proc assert {condition errMsg} {
        if {![uplevel 1 [list expr $condition]]} {
            error $errMsg
        }
    }
    foreach proc $procs {
        set actual [$proc $text]
        assert {$actual eq $acronym}  "wrong result for $proc:\n actual=$actual\n expect=$acronym"
    }

    # benchmarking
    set w [tcl::mathfunc::max {*}[lmap p $procs {string length $p}]]
    set repetitions 10000
    proc dotest {desc p args} {
        foreach var {w repetitions} {upvar $var $var}
        puts -nonewline [format {%-*s  %-12s  } $w $p $desc]
        flush stdout
        set t [time {$p {*}$args} $repetitions]
        set units [lassign $t sec]
        puts [format {%10.3f %s} $sec $units]
    }
    foreach proc $procs {
        dotest "lorem ipsum" $proc $text
    }
}

if {$argv0 eq [info script]} then benchmark
