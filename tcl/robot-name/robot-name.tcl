oo::class create Robot {
    variable name

    constructor {} {
        my reset
    }

    method reset {} {set name [RobotNames nextName]}
    method name {} {return $name}
}

proc resetRobotNames {} {
    RobotNames reset
}

# using a namespace to hold the global list of available robot names.  
namespace eval RobotNames {
    namespace export nextName reset
    namespace ensemble create

    proc generate {} {
        variable names
        variable index
        variable size

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
        set size [llength $names]
        return
    }

    generate

    proc nextName {} {
        variable names
        variable index
        variable size

        if {$index >= $size} {
            error "all names in use"
        }
        set name [lindex $names $index]
        incr index
        return $name
    }

    proc reset {} {
        variable index
        variable names
        set index 0
        #shuf names
        return
    }
}

# ref: https://wiki.tcl-lang.org/page/Shuffle+a+list
proc shuf {listvar} {
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

