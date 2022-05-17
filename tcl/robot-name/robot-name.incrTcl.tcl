package require itcl

# incr Tcl has 
# * class variables => "common"
# * class methods   => "proc"
# * access protection => public/protected/private

itcl::class iRobot {
    private {
        common allNames {}
        common nextNameIndex 0
        variable name
    }

    constructor {} {
        # the list of all names is created when the first 
        # object is instantiated
        if {[llength $allNames] == 0} {
            generateNames
        }
        set name [nextName]
    }

    method name {} {
        return $name
    }

    method reset {} {
        set name [nextName]
    }

    # The static methods
    #
    private proc generateNames {} {
        # generate all possible names
        set start [clock milliseconds]
        foreach a {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z} {
            foreach b {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z} {
                for {set c 0} {$c < 1000} {incr c} {
                    lappend allNames [format "%s%s%03d" $a $b $c]
                }
            }
        }
        # shuffle the list
        for {set idx1 [expr {[llength $allNames] - 1}]} {$idx1 > 0} {incr idx1 -1} {
            set idx2 [expr {int(($idx1 + 1) * rand())}]
            set temp [lindex $allNames $idx1]
            lset allNames $idx1 [lindex $allNames $idx2]
            lset allNames $idx2 $temp
        }
        puts "debug: [llength $allNames] names generated"
    }

    private proc nextName {} {
        if {$nextNameIndex >= [llength $allNames]} {
            error "All robot names have been used."
        }
        set n [lindex $allNames $nextNameIndex]
        incr nextNameIndex
        puts "debug: that was name #$nextNameIndex"
        return $n
    }

    proc resetNames {} {
        set nextNameIndex 0
    }
}

# Since incrTcl objects are instantiated like `Robot objName`,
# we need a helper proc to create with `Robot new`

proc Robot {subcommand} {
    if {$subcommand eq "new"} {
        return [iRobot #auto]
    } else {
        error "unknown Robot subcommand"
    }
}

proc resetRobotNames {} {
    iRobot::resetNames
}


############################################################
# benchmarking:
#
# 1. using the incr Tcl class
#
#    ++++ robot-1 took 830 ms
#    ++++ robot-2 took 0 ms
#    ++++ robot-3 took 0 ms
#    ++++ robot-4 took 0 ms
#    ++++ robot-5 took 0 ms
#    ++++ robot-6 took 0 ms
#    ++++ robot-7 took 0 ms
#    ++++ robot-8 took 0 ms
#    ++++ robot-9 took 2727 ms
#    ++++ robot-10 took 19552 ms
#    robot-name.test:	Total	10	Passed	10	Skipped	0	Failed	0
#
# 2. using the reference solution (first ~830 ms happens before first test)
#
#    ++++ robot-1 took 0 ms
#    ++++ robot-2 took 0 ms
#    ++++ robot-3 took 0 ms
#    ++++ robot-4 took 0 ms
#    ++++ robot-5 took 0 ms
#    ++++ robot-6 took 0 ms
#    ++++ robot-7 took 0 ms
#    ++++ robot-8 took 0 ms
#    ++++ robot-9 took 595 ms
#    ++++ robot-10 took 4464 ms
#    robot-name.test:	Total	10	Passed	10	Skipped	0	Failed	0
