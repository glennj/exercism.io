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
# Nowhere except the source code I think, but we can ask tcl to show us:
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
############################################################


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
    #   set lst "a {b c"    ;# invalid list     #}
    #   llength $lst
    variable ListValue         {TCL VALUE LIST}

    variable ClassExists       {TCL OO OVERWRITE_OBJECT}
}
