oo::class create Robot {
    variable name

    constructor {} {
        my reset
    }

    method reset {} {set name [RobotNames::nextName]}
    method name {} {return $name}
}

# using a namespace to hold the global list of available robot names.  
namespace eval RobotNames {
    namespace export generate nextName

    proc generate {} {
        variable names
        variable index
        variable count

        set names {}
        set index 0
        set alphabet {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z}

        foreach a $alphabet {
            foreach b $alphabet {
                for {set c 0} {$c < 1000} {incr c} {
                    lappend names [format {%s%s%03d} $a $b $c]
                }
            }
        }
        # shuffle names
        set count [llength $names]
        return
    }

    proc nextName {} {
        variable names
        variable index
        variable count

        if {$index >= $count} {
            error "all names in use"
        }
        set name [lindex $names $index]
        incr index
        return $name
    }
}

proc resetRobotNames {} {RobotNames::generate}

# ref: https://wiki.tcl-lang.org/page/Shuffle+a+list
proc shuffle {listvar} {
    upvar 1 $listvar list
    set n [llength $list]
    for {set i [expr {$n - 1}]} {$i > 0} {incr i -1} {
        set j [expr {int(rand() * ($i + 1))}]
        set temp [lindex $list $i]
        lset list $i [lindex $list $j]
        lset list $j $temp
    }
    return
}

