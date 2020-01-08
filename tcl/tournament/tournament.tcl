proc tournamentResults {filename} {
    [Tournament new $filename] results
}

# ---------------------------------------------------------
oo::class create Tournament {
    variable filename teams

    constructor {aFilename} {
        set filename $aFilename
        set teams [dict create]
    }

    method results {} {
        my Parse
        my Format
    }

    method Parse {} {
        set fh [open $filename r]
        while {[gets $fh match] != -1} {
            if {[regexp {(.+);(.+);(.+)} $match -> home away result]} {
                my InitializeTeam $home
                my InitializeTeam $away
                my RegisterMatch $home $away $result
            }
        }
        close $fh
    }

    method InitializeTeam {team} {
        if {![dict exists $teams $team]} {
            dict set teams $team [Team new $team]
        }
    }

    method RegisterMatch {home away result} {
        switch -exact -- $result {
            win {
                [dict get $teams $home] win
                [dict get $teams $away] lose
            }
            loss {
                [dict get $teams $home] lose
                [dict get $teams $away] win
            }
            draw {
                [dict get $teams $home] draw
                [dict get $teams $away] draw
            }
        }
    }

    method Format {} {
        set fmt "%-30s | %2s | %2s | %2s | %2s | %2s"
        lappend output [format $fmt Team MP W D L P]

        # sort by points descending then by name ascending
        set standings [lmap team [dict values $teams] {$team asList}]
        set standings [lsort -index 0 -dictionary $standings]
        set standings [lsort -index end -integer -decreasing $standings]

        foreach team $standings {
            lappend output [format $fmt {*}$team]
        }

        join $output \n
    }
}

# ---------------------------------------------------------
oo::class create Team {
    variable name wins losses draws 

    constructor {aName} {
        set name $aName
        set wins [set losses [set draws 0]]
    }

    method win  {} {incr wins}
    method draw {} {incr draws}
    method lose {} {incr losses}

    method asList {} {
        set played [expr {$wins + $draws + $losses}]
        set points [expr {3 * $wins + 1 * $draws}]
        list $name $played $wins $draws $losses $points
    }
}
