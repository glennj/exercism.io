proc transform {rawInput} {
    try {
        set input [dict create {*}$rawInput]
    } on error {} {
        error "invalid input"
    }
    
    set output [dict create]
    dict for {value letters} $input {
        foreach letter $letters {
            dict set output [string tolower $letter] $value
        }
    }
    return $output
}
