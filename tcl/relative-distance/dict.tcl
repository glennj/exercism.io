# Monkeypatching a new subcommand into the builtin `dict` command.

proc ::tcl::dict::setIfAbsent {dictVarName key value} {
    upvar 1 $dictVarName dictVar
    # `set` is a command in the ::tcl::dict namespace, so qualify it
    if {![dict exists $dictVar $key]} {
        dict set dictVar $key $value
    }
    return
}
namespace ensemble configure dict -map [concat \
    [namespace ensemble configure dict -map] \
    setIfAbsent ::tcl::dict::setIfAbsent ]

