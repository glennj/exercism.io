# Since the DOT language is *close* to Tcl syntax, I'll handle
# the incoming file as Tcl file. We'll create a `graph` proc
# to handle the data instead of working harder to parse it.
#
# This has the potential for executing malicious code. We can
# use Tcl's "safe interpreter" to help with this.

namespace eval DotFile {
    namespace export processDotFile

    proc processDotFile {filename} {
        if {![file readable $filename]} {
            error "cannot read file"
        }

        set fh [open $filename r]
        set contents [read $fh]
        close $fh

        set safe [::safe::interpCreate]

        # put the "graph" command into the safe interpreter 
        $safe alias graph [namespace current]::parseDefinition
        
        $safe eval $contents
    }

    proc parseDefinition {definition} {
        set graph [dict create nodes {} edges {} attrs {}]
        foreach line [split $definition \n] {
            lassign [normalizeLine $line] words attrib

            # skip empty lines
            if {[llength $words] == 0 && [llength $attrib] == 0} {
                continue
            }

            # skip comments
            if {[regexp {^//|#} [lindex $words 0]]} {
                continue
            }

            switch -exact -- [llength $words] {
                      0 { addAttribute $attrib }
                      1 { addNode [lindex $words 0] $attrib }
                default { addEdge $words $attrib }
            }
        }
        return $graph
    }

    proc normalizeLine {line} {
        set line [string trim $line]

        # remove trailing semicolon
        regsub {;$} $line "" line

        # parse the attribute
        set attr {}
        if {[regexp {\s*\[(.*?)\]$} $line attribText attrib]} {
            if {![regexp {^(.+?)=(.+)$} $attrib -> key value]} {
                error "invalid attribute"
            }
            set attr [list $key [regsub {^"(.+)"$} $value {\1}]]
            set line [string range $line 0 end-[string length $attribText]]
        }

        return [list [split $line] $attr]
    }

    proc addAttribute {attrib} {
        upvar graph graph
        dict set graph attrs {*}$attrib
    }

    proc addNode {node attrib} {
        upvar graph graph
        if {[regexp {\W} $node]} {
            error "node name must be alphanumeric"
        }
        set nodeAttrs [dict getdef $graph nodes $node {}]
        if {[llength $attrib] > 0} {
            dict set nodeAttrs {*}$attrib
        }
        dict set graph nodes $node $nodeAttrs
    }

    proc addEdge {words attrib} {
        upvar graph graph
        while {[llength $words] > 1} {
            # unshift 3 elements off the words list
            set words [lassign $words a -> b]

            if {${->} ne "--" || $b eq ""} {
                error "invalid edge"
            }

            addNode $a {}
            addNode $b {}

            set edge [lsort [list $a $b]]
            set edgeAttrs [dict getdef $graph edges $edge {}]
            if {[llength $attrib] > 0} {
                dict set edgeAttrs {*}$attrib
            }
            dict set graph edges $edge $edgeAttrs

            # shift the 2nd node back into the list
            set words [linsert $words 0 $b]
        }
    }
}

namespace import DotFile::processDotFile
