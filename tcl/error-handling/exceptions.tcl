# Work in progress: map Java-type exception names to
# something that Tcl can use in a `try ... trap ...` command
#
# usage:
#       package require Exceptions
#
#       try {
#           set fh [open $someFile r]
#       } trap {POSIX ENOENT} {err opts} {
#           some error handler
#       } trap $Exceptions::FileNotFound {err opts} {
#           same as above, but hopefully more clear
#       }

package provide Exceptions 0.1

namespace eval ::Exceptions {
    # IO errors
    variable FileAlreadyExists {POSIX EEXISTS}
    variable FileNotFound      {POSIX ENOENT}

    # Arithmetic
    variable DivisionByZero    {ARITH DIVZERO}
    variable WrongType         {ARITH DOMAIN}

    # example of WrongType:
    #   % expr {pow(-1, 0.5)}
    #   domain error: argument not in valid range

    # Tcl

    # example: puts a b c
    variable Arguments         {TCL WRONGARGS}

    # example: read foobar
    variable ChannelNotOpen    {TCL LOOKUP CHANNEL}

    variable NoSuchCommand     {TCL LOOKUP COMMAND}
    variable NoSuchVariable    {TCL LOOKUP VARIABLE}

    # example:
    #   unset x
    #   set search_id [array startsearch x]
    variable NotAnArray        {TCL LOOKUP VARNAME}

    # example: 
    #   set d {1 2 3}   ;# invalid dictionary
    #   dict size $d
    variable DictionaryKey     {TCL VALUE DICTIONARY}

    # example:
    #   set lst "a {b c"    ;# invalid list
    #   llength $lst
    variable ListValue         {TCL VALUE LIST}

    variable ClassExists       {TCL OO OVERWRITE_OBJECT}
}
