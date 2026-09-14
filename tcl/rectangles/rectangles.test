#!/usr/bin/env tclsh
# generated: 2026-07-23T12:41:57Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "rectangles.tcl"


test rectangles-1 "no rows" -body {
    rectangles {}
} -returnCodes ok -result 0

skip rectangles-2
test rectangles-2 "no columns" -body {
    rectangles {
        ""
    }
} -returnCodes ok -result 0

skip rectangles-3
test rectangles-3 "no rectangles" -body {
    rectangles {
        " "
    }
} -returnCodes ok -result 0

skip rectangles-4
test rectangles-4 "one rectangle" -body {
    rectangles {
        "+-+"
        "| |"
        "+-+"
    }
} -returnCodes ok -result 1

skip rectangles-5
test rectangles-5 "two rectangles without shared parts" -body {
    rectangles {
        "  +-+"
        "  | |"
        "+-+-+"
        "| |  "
        "+-+  "
    }
} -returnCodes ok -result 2

skip rectangles-6
test rectangles-6 "five rectangles with shared parts" -body {
    rectangles {
        "  +-+"
        "  | |"
        "+-+-+"
        "| | |"
        "+-+-+"
    }
} -returnCodes ok -result 5

skip rectangles-7
test rectangles-7 "rectangle of height 1 is counted" -body {
    rectangles {
        "+--+"
        "+--+"
    }
} -returnCodes ok -result 1

skip rectangles-8
test rectangles-8 "rectangle of width 1 is counted" -body {
    rectangles {
        "++"
        "||"
        "++"
    }
} -returnCodes ok -result 1

skip rectangles-9
test rectangles-9 "1x1 square is counted" -body {
    rectangles {
        "++"
        "++"
    }
} -returnCodes ok -result 1

skip rectangles-10
test rectangles-10 "only complete rectangles are counted" -body {
    rectangles {
        "  +-+"
        "    |"
        "+-+-+"
        "| | -"
        "+-+-+"
    }
} -returnCodes ok -result 1

skip rectangles-11
test rectangles-11 "rectangles can be of different sizes" -body {
    rectangles {
        "+------+----+"
        "|      |    |"
        "+---+--+    |"
        "|   |       |"
        "+---+-------+"
    }
} -returnCodes ok -result 3

skip rectangles-12
test rectangles-12 "corner is required for a rectangle to be complete" -body {
    rectangles {
        "+------+----+"
        "|      |    |"
        "+------+    |"
        "|   |       |"
        "+---+-------+"
    }
} -returnCodes ok -result 2

skip rectangles-13
test rectangles-13 "large input with many rectangles" -body {
    rectangles {
        "+---+--+----+"
        "|   +--+----+"
        "+---+--+    |"
        "|   +--+----+"
        "+---+--+--+-+"
        "+---+--+--+-+"
        "+------+  | |"
        "          +-+"
    }
} -returnCodes ok -result 60

skip rectangles-14
test rectangles-14 "rectangles must have four sides" -body {
    rectangles {
        "+-+ +-+"
        "| | | |"
        "+-+-+-+"
        "  | |  "
        "+-+-+-+"
        "| | | |"
        "+-+ +-+"
    }
} -returnCodes ok -result 5


cleanupTests
