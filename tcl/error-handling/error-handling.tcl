lappend auto_path .
package require Exceptions

proc handle_error {script} {
    set result "success"
    try {
        uplevel 1 $script 
    } trap $::Exceptions::DivisionByZero {err opts} {
        set result "division by zero"
    } trap $::Exceptions::FileNotFound {err opts} {
        set result "file does not exist"
    } trap $::Exceptions::NoSuchCommand {err opts} {
        set result "proc does not exist"
    } on error {errMsg errOpts} {
        set result "unknown error: [list [dict get $errOpts -errorcode] $errMsg]"
    }
    return $result
}

# `custom_error_message` -- just delegate to `error`
interp alias "" custom_error_message "" error
