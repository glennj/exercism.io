# Monkeypatching a new subcommand into the builtin `dict` command.
#
#    dict getdef dictionaryValue ?key ...? key default
#
# This will be in Tcl 8.7 -- https://tcl.tk/man/tcl8.7/TclCmd/dict.htm
#
proc __tcl_dict_getdef {dictValue args} {
    set default [lindex $args end]
    set keys [lrange $args 0 end-1]
    if {[dict exists $dictValue {*}$keys]} {
        return [dict get $dictValue {*}$keys]
    } else {
        return $default
    }
}
namespace ensemble configure dict -map [concat \
    [namespace ensemble configure dict -map] \
    getdef __tcl_dict_getdef ]

