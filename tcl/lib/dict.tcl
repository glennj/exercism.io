# Monkeypatching a new subcommand into the builtin `dict` command.
# `dict getdef` will be in Tcl 8.7
#
proc ::tcl::dict::getdef {dictValue key default} {
    if {[dict exists $dictValue $key]} {
        return [dict get $dictValue $key]
    } else {
        return $default
    }
}

namespace ensemble configure dict -map [concat \
    [namespace ensemble configure dict -map] \
    getdef ::tcl::dict::getdef ]

