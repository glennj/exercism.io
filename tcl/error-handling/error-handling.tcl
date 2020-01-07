proc custom_error_message {message} {
    error $message
}

proc handle_error {script} {
    set result "success"
    try {
        uplevel 1 $script 
    } trap {ARITH DIVZERO} err {
        set result "division by zero"
    } trap {POSIX ENOENT} err {
        set result "file does not exist"
    } trap {TCL LOOKUP COMMAND} err {
        set result "proc does not exist"
    } on error {errMsg errOpts} {
        set result "unknown error: [list [dict get $errOpts -errorcode] $errMsg]"
    }
    return $result
}
