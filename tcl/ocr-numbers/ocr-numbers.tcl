source "./dict.tcl"


namespace eval OCR {
    namespace export convert

    variable numbers {
        " _ | ||_|   " 0
        "     |  |   " 1
        " _  _||_    " 2
        " _  _| _|   " 3
        "   |_|  |   " 4
        " _ |_  _|   " 5
        " _ |_ |_|   " 6
        " _   |  |   " 7
        " _ |_||_|   " 8
        " _ |_| _|   " 9
    }

    proc convert {rows} {
        set height [llength $rows]
        if {$height == 0} then {return ""}
        set width [string length [lindex $rows 0]]

        assert {$height % 4 == 0} "Number of input lines is not a multiple of four"
        assert {$width % 3 == 0} "Number of input columns is not a multiple of three"
        assert {[all row $rows {[string length $row] == $width}]} "Lines are not all the same length"

        set results {}
        for {set r 0} {$r < $height} {incr r 4} {
            set digits ""
            for {set c 0} {$c < $width} {incr c 3} {
                append digits [convertDigit $rows $r $c]
            }
            lappend results $digits
        }

        return [join $results ,]
    }

    proc convertDigit {rows r c} {
        variable numbers
        set digitString [join [lmap row [lrange $rows $r $r+3] {
            string range $row $c $c+2
        }] ""]
        return [dict getdef $numbers $digitString "?"]
    }
}

############################################################
proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

proc all {elemName list condition} {
    upvar 1 $elemName elem
    foreach elem $list {
        if {![uplevel 1 [list expr $condition]]} {
            return false
        }
    }
    return true
}

############################################################
namespace import OCR::convert
