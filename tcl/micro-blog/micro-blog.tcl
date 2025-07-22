package require Tcl 9.0

proc truncate {input {size 5}} {
    string range $input 0 $size-1
}
