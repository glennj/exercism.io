# The first argument has a default value.
# ref: https://tcl.tk/man/tcl8.6/TclCmd/proc.htm
#
proc two-fer {{name you}} {
    return "One for $name, one for me."
}
