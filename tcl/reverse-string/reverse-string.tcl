#############################################################
# the "forbidden" solution
proc stringReverse {input} {
    return [string reverse $input]
}

#############################################################
# similarly, cheating
proc stringReverse {input} {
    return [join [lreverse [split $input ""]] ""]
}

#############################################################
# a straightforward solution
proc stringReverse {input} {
    set output ""
    while {$input ne ""} {
        append output [string index $input end]
        set input [string range $input 0 end-1]
    }
    return $output
}

#############################################################
# A bit trickier

# do {some action} while  {some condition}
# do {some action} until  {some condition}
# do {some action} if     {some condition}
# do {some action} unless {some condition}
#
proc do {body keyword condition} {
    switch -exact -- $keyword {
        if     {}
        while  {uplevel $body}
        unless {
            set keyword "if"
            set condition "!($condition)"
        }
        until  {
            set keyword "while"
            set condition "!($condition)"
            uplevel $body
        }
        default {
            error "unknown keyword \"$keyword\": must be if, unless, while or until"
        }
    }
    $keyword {[uplevel [list expr $condition]]} {
        uplevel $body
    }
}

proc stringReverse {input} {
    do {
        append output [string index $input end]
        set input [string range $input 0 end-1]
    } while {$input ne ""}
    return $output
}


#############################################################
# Let's get fancy
#
# create a new "string" subcommand: foreach
namespace ensemble configure "string" -map [list    \
    {*}[namespace ensemble configure "string" -map] \
    "foreach" [list ::apply {{varname string body} {
        ::upvar 1 $varname char
        ::foreach char [::split $string ""] {
            ::uplevel 1 $body
        }
    }}] \
]

# an analog to `append`
proc prepend {varname prefix} {
    upvar 1 $varname str
    # ref: "Unsharing objects" at https://wiki.tcl.tk/K
    set str [string cat $prefix $str[set str ""]]
}


proc stringReverse {input} {
    set output ""
    string foreach char $input {prepend output $char}
    return $output
}
