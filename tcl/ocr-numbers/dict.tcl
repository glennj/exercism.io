# Monkeypatching a new subcommand into the builtin `dict` command.
#
#    dict getdef dictionaryValue ?key ...? key default
#
# This will be in Tcl 8.7 -- https://tcl.tk/man/tcl8.7/TclCmd/dict.htm
#
proc ::tcl::dict::getdef {dictValue args} {
    # `set` is a command in the ::tcl::dict namespace, so qualify it
    ::set default [lindex $args end]
    ::set keys [lrange $args 0 end-1]
    if {[dict exists $dictValue {*}$keys]} {
        return [dict get $dictValue {*}$keys]
    } else {
        return $default
    }
}
namespace ensemble configure dict -map [concat \
    [namespace ensemble configure dict -map] \
    getdef ::tcl::dict::getdef ]

