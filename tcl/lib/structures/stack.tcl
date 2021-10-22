package provide structures::stack 0.1

# a Stack is not Sequenceable

oo::class create Stack {
    variable data

    constructor {} {
        set data [list]
    }

    method push {element} {
        lappend data $element
        return [self]
    }

    method pop {} {
        set element [lindex $data end]
        set data [lrange $data[set data ""] 0 end-1]
        return $element
    }

    method peek {} {
        return [lindex $data end]
    }

    method size {} {
        return [llength $data]
    }

    method isEmpty {} {
        return [expr {[my size] == 0} ]
    }

    method contents {} {
        return $data
    }
}
