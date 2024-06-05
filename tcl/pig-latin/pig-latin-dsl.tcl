# alias the builtins used
# I would have used `unknown`, but `eturnray` proved too hard to get right.
interp alias {} oreachfay {} foreach
interp alias {} ifay      {} if
interp alias {} oinjay    {} join
interp alias {} aplmay    {} lmap
interp alias {} ocpray    {} proc
interp alias {} eturnray  {} return
interp alias {} etsay     {} set

# for `namespace` handle piggified subcommands
proc amespacenay {subcommand args} {
    set subcommand_map [namespace ensemble configure namespace -map]
    set subcommands [dict keys $subcommand_map]
    uplevel 1 [list namespace [unpiggify $subcommand $subcommands] {*}$args]
}

# for `regexp` and `regsub` handle piggified -options
proc egexpray {args} {
    uplevel 1 [list regexp {*}[unpiggify_options regexp {*}$args]]
}

proc egsubray {args} {
    uplevel 1 [list regsub {*}[unpiggify_options regsub {*}$args]]
}

# Translate _from_ pig latin _to_ english,
# with some knowledge about the subcommands and option names in Tcl,
# and how many prefix chars get moved to the end of the word.
# For example, there is no "squelch" word.
proc unpiggify {ordway words} {
    if {[regexp {(.*)(.)(.)ay$} $ordway -> a b c]} {
        foreach candidate [list "$a$b$c" "$c$a$b" "$b$c$a"] {
            if {$candidate in $words} {
                return $candidate
            }
        }
    }
    return $ordway
}

proc unpiggify_options {command args} {
    # capture the error message for an invalid option
    catch {$command -####} option_string
    # extract the option names
    set options [lmap {_ option} [regexp -all -inline -- { -(\w+)} $option_string] {string cat $option}]

    return [lmap arg $args {
        if {[regexp -- {^-(.+)} $arg -> opt]} {
            string cat - [unpiggify $opt $options]
        } else {
            string cat $arg
        }
    }]
}
