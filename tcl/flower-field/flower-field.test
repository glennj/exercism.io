#!/usr/bin/env tclsh
# generated: 2026-07-24T19:22:55Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "flower-field.tcl"


test flower-field-1 "no rows" -body {
    annotate {}
} -returnCodes ok -match orderedLists -result {}

skip flower-field-2
test flower-field-2 "no columns" -body {
    annotate {
        ""
    }
} -returnCodes ok -match orderedLists -result {
        ""
    }

skip flower-field-3
test flower-field-3 "no flowers" -body {
    annotate {
        "   "
        "   "
        "   "
    }
} -returnCodes ok -match orderedLists -result {
        "   "
        "   "
        "   "
    }

skip flower-field-4
test flower-field-4 "garden full of flowers" -body {
    annotate {
        "***"
        "***"
        "***"
    }
} -returnCodes ok -match orderedLists -result {
        "***"
        "***"
        "***"
    }

skip flower-field-5
test flower-field-5 "flower surrounded by spaces" -body {
    annotate {
        "   "
        " * "
        "   "
    }
} -returnCodes ok -match orderedLists -result {
        "111"
        "1*1"
        "111"
    }

skip flower-field-6
test flower-field-6 "space surrounded by flowers" -body {
    annotate {
        "***"
        "* *"
        "***"
    }
} -returnCodes ok -match orderedLists -result {
        "***"
        "*8*"
        "***"
    }

skip flower-field-7
test flower-field-7 "horizontal line" -body {
    annotate {
        " * * "
    }
} -returnCodes ok -match orderedLists -result {
        "1*2*1"
    }

skip flower-field-8
test flower-field-8 "horizontal line, flowers at edges" -body {
    annotate {
        "*   *"
    }
} -returnCodes ok -match orderedLists -result {
        "*1 1*"
    }

skip flower-field-9
test flower-field-9 "vertical line" -body {
    annotate {
        " "
        "*"
        " "
        "*"
        " "
    }
} -returnCodes ok -match orderedLists -result {
        "1"
        "*"
        "2"
        "*"
        "1"
    }

skip flower-field-10
test flower-field-10 "vertical line, flowers at edges" -body {
    annotate {
        "*"
        " "
        " "
        " "
        "*"
    }
} -returnCodes ok -match orderedLists -result {
        "*"
        "1"
        " "
        "1"
        "*"
    }

skip flower-field-11
test flower-field-11 "cross" -body {
    annotate {
        "  *  "
        "  *  "
        "*****"
        "  *  "
        "  *  "
    }
} -returnCodes ok -match orderedLists -result {
        " 2*2 "
        "25*52"
        "*****"
        "25*52"
        " 2*2 "
    }

skip flower-field-12
test flower-field-12 "large garden" -body {
    annotate {
        " *  * "
        "  *   "
        "    * "
        "   * *"
        " *  * "
        "      "
    }
} -returnCodes ok -match orderedLists -result {
        "1*22*1"
        "12*322"
        " 123*2"
        "112*4*"
        "1*22*2"
        "111111"
    }

skip flower-field-13
test flower-field-13 "multiple adjacent flowers" -body {
    annotate {
        " ** "
    }
} -returnCodes ok -match orderedLists -result {
        "1**1"
    }


cleanupTests
