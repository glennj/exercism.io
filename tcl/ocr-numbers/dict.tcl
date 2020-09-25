# Monkeypatching a new subcommand into the builtin `dict` command.
#
#    dict getdef dictionaryValue ?key ...? key default
#
# This will be in Tcl 8.7 -- https://tcl.tk/man/tcl8.7/TclCmd/dict.htm

set dictSubcommands [namespace ensemble configure dict -map]

if {![dict exists $dictSubcommands getdef]} {
    proc ::tcl::dict::getdef {dictValue args} {
        if {[llength $args] < 2} {
            error {wrong # args: should be "dict getdef dictionary ?key ...? key default"}
        }

        # `set` is a command in the ::tcl::dict namespace, 
        # so this usage needs to be fully qualified
        ::set default [lindex $args end]
        ::set keys [lrange $args 0 end-1]

        if {[dict exists $dictValue {*}$keys]} {
            return [dict get $dictValue {*}$keys]
        } else {
            return $default
        }
    }

    dict set dictSubcommands getdef ::tcl::dict::getdef
    namespace ensemble configure dict -map $dictSubcommands
}

unset dictSubcommands
