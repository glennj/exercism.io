package require Tcl 8.5
package require cmdline     ;# from tcllib

namespace eval ::fileutil::grep {
    namespace export grep

    ########################################
    proc grep {args} {
        lassign [parseArgs $args] opts pattern files
        set result {}
        foreach file $files {
            lappend result {*}[grepFile $file $pattern $opts]
        }
        join $result \n
    }

    ########################################
    proc parseArgs {arguments} {
        set opts [::cmdline::getoptions arguments {
            {i "case insensitive"}
            {l "filename only"}
            {n "with line numbers"}
            {v "invert match"}
            {x "whole line"}
        } {: grep [options] pattern file ...}]

        # remaining args are the pattern and the files
        set files [lassign $arguments pattern]

        if {[dict get $opts x]} then {set pattern "^${pattern}$"}
        if {[dict get $opts i]} then {set pattern [string tolower $pattern]}
        dict set opts nfiles [llength $files]

        list $opts $pattern $files
    }

    ########################################
    proc grepFile {file pattern opts} {
        set fh [open $file r]
        set nr 0
        set result {}
        while {[gets $fh line] != -1} {
            incr nr
            if {[isMatch $line $pattern $opts]} {
                if {[dict get $opts l]} {
                    lappend result $file
                    break
                } else {
                    lappend result [formatMatch $file $nr $line $opts]
                }
            }
        }
        close $fh
        return $result
    }

    ########################################
    proc isMatch {line pattern opts} {
        if {[dict get $opts i]} {
            set line [string tolower $line]
        }
        expr {[regexp $pattern $line] ^ [dict get $opts v]}
    }

    ########################################
    proc formatMatch {file nr line opts} {
        set result {}
        if {[dict get $opts nfiles] > 1} {
            lappend result $file
        }
        if {[dict get $opts n]} {
            lappend result $nr
        }
        lappend result $line
        join $result ":"
    }
}


namespace import ::fileutil::grep::grep
