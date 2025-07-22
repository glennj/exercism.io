oo::class create Stopwatch {
    variable lapDurations
    variable prevDuration
    variable laps
    variable lapStart
    variable state
    
    constructor {} {
        my reset
    }

    method reset {} {
        if {[info exists state] && $state ne "stopped"} {
            error "cannot reset a stopwatch that is not stopped"
        }
        set lapDurations {}
        set prevDuration 0
        set laps {}
        set state ready
        return
    }

    method state {} {
        return $state
    }

    method previousLaps {} {
        return $laps
    }

    method start {} {
        if {$state eq "running"} {
            error "cannot start an already running stopwatch"
        }
        set state running
        set lapStart [clock seconds]
        return
    }

    method stop {} {
        if {$state ne "running"} {
            error "cannot stop a stopwatch that is not running"
        }
        set state stopped
        lappend lapDurations [expr {[clock seconds] - $lapStart}]
        return
    }

    method lap {} {
        if {$state ne "running"} {
            error "cannot lap a stopwatch that is not running"
        }
        set lapDuration [my CurrentLapDuration]
        lappend laps [my FormatTime $lapDuration]
        incr prevDuration $lapDuration
        set lapDurations {}
        set lapStart [clock seconds]
        return
    }

    method total {} {
        set seconds [expr {[my CurrentLapDuration] + $prevDuration}]
        return [my FormatTime $seconds]
    }

    method currentLap {} {
        return [my FormatTime [my CurrentLapDuration]]
    }

    method CurrentLapDuration {} {
        set seconds [::tcl::mathop::+ {*}$lapDurations]
        if {$state eq "running"} {
            incr seconds [expr {[clock seconds] - $lapStart}]
        }
        return $seconds
    }

    method FormatTime {duration} {
        set secs [expr {$duration % 60}]
        set mins [expr {$duration / 60 % 60}]
        set hrs  [expr {$duration / 60 / 60}]
        return [format {%02d:%02d:%02d} $hrs $mins $secs]
    }
}
