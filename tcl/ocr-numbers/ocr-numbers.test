#!/usr/bin/env tclsh
# generated: 2026-07-18T19:02:50Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "ocr-numbers.tcl"


test ocr-numbers-1 "Recognizes 0" -body {
    convert {
        " _ "
        "| |"
        "|_|"
        "   "
    }
} -returnCodes ok -result "0"

skip ocr-numbers-2
test ocr-numbers-2 "Recognizes 1" -body {
    convert {
        "   "
        "  |"
        "  |"
        "   "
    }
} -returnCodes ok -result "1"

skip ocr-numbers-3
test ocr-numbers-3 "Unreadable but correctly sized inputs return ?" -body {
    convert {
        "   "
        "  _"
        "  |"
        "   "
    }
} -returnCodes ok -result "?"

skip ocr-numbers-4
test ocr-numbers-4 "Input with a number of lines that is not a multiple of four raises an error" -body {
    convert {
        " _ "
        "| |"
        "   "
    }
} -returnCodes error -result "Number of input lines is not a multiple of four"

skip ocr-numbers-5
test ocr-numbers-5 "Input with a number of columns that is not a multiple of three raises an error" -body {
    convert {
        "    "
        "   |"
        "   |"
        "    "
    }
} -returnCodes error -result "Number of input columns is not a multiple of three"

skip ocr-numbers-6
test ocr-numbers-6 "Recognizes 110101100" -body {
    convert {
        "       _     _        _  _ "
        "  |  || |  || |  |  || || |"
        "  |  ||_|  ||_|  |  ||_||_|"
        "                           "
    }
} -returnCodes ok -result "110101100"

skip ocr-numbers-7
test ocr-numbers-7 "Garbled numbers in a string are replaced with ?" -body {
    convert {
        "       _     _           _ "
        "  |  || |  || |     || || |"
        "  |  | _|  ||_|  |  ||_||_|"
        "                           "
    }
} -returnCodes ok -result "11?10?1?0"

skip ocr-numbers-8
test ocr-numbers-8 "Recognizes 2" -body {
    convert {
        " _ "
        " _|"
        "|_ "
        "   "
    }
} -returnCodes ok -result "2"

skip ocr-numbers-9
test ocr-numbers-9 "Recognizes 3" -body {
    convert {
        " _ "
        " _|"
        " _|"
        "   "
    }
} -returnCodes ok -result "3"

skip ocr-numbers-10
test ocr-numbers-10 "Recognizes 4" -body {
    convert {
        "   "
        "|_|"
        "  |"
        "   "
    }
} -returnCodes ok -result "4"

skip ocr-numbers-11
test ocr-numbers-11 "Recognizes 5" -body {
    convert {
        " _ "
        "|_ "
        " _|"
        "   "
    }
} -returnCodes ok -result "5"

skip ocr-numbers-12
test ocr-numbers-12 "Recognizes 6" -body {
    convert {
        " _ "
        "|_ "
        "|_|"
        "   "
    }
} -returnCodes ok -result "6"

skip ocr-numbers-13
test ocr-numbers-13 "Recognizes 7" -body {
    convert {
        " _ "
        "  |"
        "  |"
        "   "
    }
} -returnCodes ok -result "7"

skip ocr-numbers-14
test ocr-numbers-14 "Recognizes 8" -body {
    convert {
        " _ "
        "|_|"
        "|_|"
        "   "
    }
} -returnCodes ok -result "8"

skip ocr-numbers-15
test ocr-numbers-15 "Recognizes 9" -body {
    convert {
        " _ "
        "|_|"
        " _|"
        "   "
    }
} -returnCodes ok -result "9"

skip ocr-numbers-16
test ocr-numbers-16 "Recognizes string of decimal numbers" -body {
    convert {
        "    _  _     _  _  _  _  _  _ "
        "  | _| _||_||_ |_   ||_||_|| |"
        "  ||_  _|  | _||_|  ||_| _||_|"
        "                              "
    }
} -returnCodes ok -result "1234567890"

skip ocr-numbers-17
test ocr-numbers-17 "Numbers separated by empty lines are recognized. Lines are joined by commas." -body {
    convert {
        "    _  _ "
        "  | _| _|"
        "  ||_  _|"
        "         "
        "    _  _ "
        "|_||_ |_ "
        "  | _||_|"
        "         "
        " _  _  _ "
        "  ||_||_|"
        "  ||_| _|"
        "         "
    }
} -returnCodes ok -result "123,456,789"


cleanupTests
