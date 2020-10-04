lappend auto_path ../lib
package require Exceptions

proc handle_error {script} {
    set result "success"
    try {
        uplevel 1 $script 
    } trap $Exceptions::DivisionByZero {err opts} {
        set result "division by zero"
    } trap $Exceptions::FileNotFound {err opts} {
        set result "file does not exist"
    } trap $Exceptions::NoSuchCommand {err opts} {
        set result "proc does not exist"
    } on error {errMsg errOpts} {
        set result "unknown error: [list [dict get $errOpts -errorcode] $errMsg]"
    }
    return $result
}


# `custom_error_message` -- just delegate to `error`
interp alias "" custom_error_message "" error


############################################################
### try ... trap
# 
# Tcl 8.6 introduced the [`try` command](http://www.tcl-lang.org/man/tcl8.6/TclCmd/try.htm).
# This encapsulates a lot of the uses of `catch`
# 
# ```tcl
# try {
#     expr {$a / $b}
# } trap {ARITH DIVZERO} {
#     puts "division by zero"
# }
# ```
# Now, where is "ARITH DIVZERO" documented? 
# Nowhere except the source code I think, but we can as tcl to show us:
#
# ```tcl
# $ tclsh
# % set a 1; set b 0
# % expr {$a / $b}
# divide by zero
# 
# % set errorInfo     ;# this is the stack trace
# divide by zero
#     while executing
# "expr {$a / $b}"
# 
# % set errorCode     ;# this is what we're after
# ARITH DIVZERO {divide by zero}
# ```
# 
# So the error to be trapped is `lrange $errorCode 0 end-1`

