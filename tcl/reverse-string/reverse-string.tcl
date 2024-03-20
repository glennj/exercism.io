namespace eval StringReverser {
    proc iterative {input} {
        set reversed ""
        set idx [string length $input]
        while {$idx > 0} {
            append reversed [string index $input [incr idx -1]]
        }
        return $reversed
    }

    proc recursive {input {reversed ""}} {
        if {$input eq ""} then {return $reversed}
        set head [string index $input 0]
        set tail [string range $input 1 end]
        tailcall recursive $tail [string cat $head $reversed]
    }
}

#interp alias {} reverse {} ::StringReverser::iterative
interp alias {} reverse {} ::StringReverser::recursive
