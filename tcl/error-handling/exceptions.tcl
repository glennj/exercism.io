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

    # Tcl
    variable Arguments         {TCL WRONGARGS}
    variable ChannelNotOpen    {TCL LOOKUP CHANNEL}
    variable NoSuchCommand     {TCL LOOKUP COMMAND}
    variable NoSuchVariable    {TCL LOOKUP VARIABLE}
    variable NotAnArray        {TCL LOOKUP VARNAME}
    variable DictionaryKey     {TCL VALUE DICTIONARY}
    variable ListValue         {TCL VALUE LIST}

    variable ClassExists       {TCL OO OVERWRITE_OBJECT}
}
