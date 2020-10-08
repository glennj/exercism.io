proc fromPov {treeInput label} {
    set pov [[Tree new $treeInput] fromPov $label]
    $pov toDict
}

proc path {treeInput fromLabel toLabel} {
    set path [[Tree new $treeInput] path $fromLabel $toLabel]
    lmap node $path {$node label}
}

############################################################
oo::class create Tree {
    variable label
    variable children

    constructor {input} {
        assert {[dict exists $input label]} "missing label: [list $input]"
        set label [dict get $input label]
        set children {}
        if {[dict exists $input children]} {
            foreach elem [dict get $input children] {
                lappend children [[self class] new $elem]
            }
        }
    }

    method label {} {return $label}

    method fromPov {label} {
        set path [my pathFromRoot $label]
        assert {[llength $path]} "no such target"

        set path [lassign $path root]
        foreach node $path {
            $root removeChild $node
            $node addChild $root
            set root $node
        }
        return $root
    }

    method path {fromLabel toLabel} {
        set fromPath [my pathFromRoot $fromLabel]
        set toPath   [my pathFromRoot $toLabel]
        assert {[llength $fromPath] && [llength $toPath]} "no such label"

        set commonPathLength -1
        foreach fromNode $fromPath toNode $toPath {
            if {$fromNode eq "" || $toNode eq "" || [$fromNode label] ne [$toNode label]} {
                break
            }
            incr commonPathLength
        }

        # the path "up"
        set path [lreverse [lrange $fromPath $commonPathLength end]]
        # the path "down"
        lappend path {*}[lrange $toPath $commonPathLength+1 end]
        return $path
    }

    method addChild {child} {
        lappend children $child
        return
    }

    method removeChild {child} {
        set idx -1
        for {set i 0} {$i < [llength $children]} {incr i} {
            if {[$child label] eq [[lindex $children $i] label]} {
                set idx $i
                break
            }
        }
        if {$idx != -1} {
            set children [lreplace $children $idx $idx]
        }
        return
    }

    method pathFromRoot {needle {path ""}} {
        set newPath [linsert $path end [self]]
        if {$needle eq $label} {
            return $newPath
        }
        foreach child $children {
            set thisPath [$child pathFromRoot $needle $newPath]
            if {$thisPath ne ""} {
                return $thisPath
            }
        }
        return
    }

    method toDict {} {
        set output {}
        dict set output label $label
        if {[llength $children] > 0} {
            foreach child $children {
                dict lappend output children [$child toDict]
            }
        }
        return $output
    }
}

############################################################
proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
