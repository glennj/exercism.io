# Refactoring notes:
#  * rename "list" variable
#  * extract the <strong> and <em> handling into procs
#  * reduce long, nested commands in strong and em procs
#  * consolidate heading handling
#  * move "if $inList" actions into a "closeList" proc
#  * for symmetry, create an "openList" proc
#  * remove the "if $inList" for paragraphs
#  * wrap in a namespace to make helper procs more private
#  * create "render*" procs to encapsulate the tags
#  * create helper "ref" proc
#
# This code still has the problem of not parsing mixed
# "__" and "_" pairs in the right order. For example,
#
#     ___hello___
#
# is rendered as
#
#     <p><strong><em>hello</strong></em></p>
#         ^^^^^^  ^^        ^^^^^^   ^^
#

namespace eval markdownParser {
    namespace export parse

    proc parse {markdownText} {
        set inList false
        set rendered ""
        foreach line [split $markdownText \n] {
            append rendered [render [em [strong $line]]]
        }
        append rendered [closeList]
        return $rendered
    }

    proc strong {line} {
        if { [set a [string first __ $line]]      == -1 ||
             [set b [string first __ $line $a+2]] == -1
        } then {
            return $line
        }

        tailcall strong [format {%s<strong>%s</strong>%s} \
            [string range $line 0    $a-1] \
            [string range $line $a+2 $b-1] \
            [string range $line $b+2 end ] ]
    }

    proc em {line} {
        set words [lassign [split $line _] html]
        foreach {emphasized plain} $words {
            append html <em> $emphasized </em> $plain
        }
        return $html
    }

    proc render {line} {
        ref inList
        
        set html [renderHeading $line]
        if {$html eq ""} then {set html [renderListItem $line]}
        if {$html eq ""} then {set html [renderParagraph $line]}

        return $html
    }

    proc renderHeading {line} {
        ref inList
        if {[regexp -expanded {^\s* (\#{1,6}) \s+ (.*)} $line -> hashes text]} {
            set tag "h[string length $hashes]"
            return "[closeList]<$tag>$text</$tag>"
        }
    }

    proc renderListItem {line} {
        ref inList
        if {[regexp -expanded {^\s* [*] \s+ (.*)} $line -> text]} {
            return "[openList]<li>$text</li>"
        }
    }

    proc renderParagraph {line} {
        ref inList
        return "[closeList]<p>$line</p>"
    }

    proc openList {} {
        ref inList
        if {!$inList} {
            set inList true
            return "<ul>"
        }
    }

    proc closeList {} {
        ref inList
        if {$inList} {
            set inList false
            return "</ul>"
        }
    }
}

namespace import markdownParser::parse

# a shortcut to reference a variable in the caller's scope
proc ref {varname} {
    uplevel 1 [list upvar 1 $varname $varname]
}
