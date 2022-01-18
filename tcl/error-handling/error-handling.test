#!/usr/bin/env tclsh
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

############################################################
source "error-handling.tcl"


test error-handling-1 {
    throw an error with the given error message
} -body {
    custom_error_message "This is an error"
} -returnCodes error -result "This is an error"


skip error-handling-2
test error-handling-2 "gracefully handle division by zero" -body {
    handle_error {expr {1 / 0}}
} -returnCodes ok -result "division by zero"

skip error-handling-3
test error-handling-3 "gracefully handle file does not exist" -body {
    handle_error {open [file join . "no dir" "no file"] r}
} -returnCodes ok -result "file does not exist"

skip error-handling-4
test error-handling-4 "gracefully handle proc does not exist" -body {
    handle_error {no::such::procedure}
} -returnCodes ok -result "proc does not exist"


skip error-handling-5
test error-handling-5 "gracefully handle success" -body {
    handle_error {append var no error here}
} -returnCodes ok -result "success"

cleanupTests
