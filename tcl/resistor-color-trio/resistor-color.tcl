namespace eval resistorColor {
    variable codes [dict create {*}{
        black   0
        brown   1
        red     2
        orange  3
        yellow  4
        green   5
        blue    6
        violet  7
        grey    8
        white   9
    }]

    proc colorCode {colour} {
        variable codes
        try {
            return [dict get $codes $colour]
        } on error {err opts} {
            error "Invalid color: $colour"
        }
    }

    proc colors {} {
        variable codes
        return [dict keys $codes]
    }
}
