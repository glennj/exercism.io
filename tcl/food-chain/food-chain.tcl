# Monkeypatching a new subcommand into the builtin `dict` command.
# Fetch a value from a dictionary, but return the default value
# if the key(s) do not exist.
#
#    dict getdef dictionaryValue ?key ...? key default
#
# This will be in Tcl 8.7 -- https://tcl.tk/man/tcl8.7/TclCmd/dict.htm
#
proc dict_getdef {dictValue args} {
    set default [lindex $args end]
    set keys [lrange $args 0 end-1]
    if {[dict exists $dictValue {*}$keys]} {
        return [dict get $dictValue {*}$keys]
    } else {
        return $default
    }
}

set dictCmds [namespace ensemble configure dict -map]
dict set dictCmds getdef dict_getdef
namespace ensemble configure dict -map $dictCmds


############################################################
namespace eval HungryOldLady {
    proc song {from to} {
        for {set i [expr {$from - 1}]} {$i < $to} {incr i} {
            lappend verses [verse $i]
        }
        return [join $verses \n\n]
    }

    variable menagerie {
        fly    {}
        spider {
            tag " that wriggled and jiggled and tickled inside her"
            rhyme "\nIt wriggled and jiggled and tickled inside her."
        }
        bird  { rhyme "\nHow absurd to swallow a bird!" }
        cat   { rhyme "\nImagine that, to swallow a cat!" }
        dog   { rhyme "\nWhat a hog, to swallow a dog!" }
        goat  { rhyme "\nJust opened her throat and swallowed a goat!" }
        cow   { rhyme "\nI don't know how she swallowed a cow!" }
        horse { lethal yes }
    }

    proc verse {n} {
        variable menagerie
        set animals [dict keys $menagerie]
        set animal [lindex $animals $n]

        set verse "I know an old lady who swallowed a $animal."

        if {[dict getdef $menagerie $animal lethal false]} {
            append verse "\nShe's dead, of course!"
        } else {
            append verse [dict getdef $menagerie $animal rhyme ""]
            set predator $animal
            for {set i [expr {$n - 1}]} {$i >= 0} {incr i -1} {
                set prey [lindex $animals $i]
                set tag [dict getdef $menagerie $prey tag ""]
                append verse "\nShe swallowed the $predator to catch the $prey$tag."
                set predator $prey
            }
            append verse "\nI don't know why she swallowed the fly. Perhaps she'll die."
        }
        return $verse
    }
}


interp alias "" recite "" HungryOldLady::song 
