package provide stack 0.1

oo::class create Stack {
    variable data

    constructor {} {
        set data [list]
    }

    method push {element} {
        set data [linsert $data[set data ""] 0 $element]
        return [self]
    }

    method pop {} {
        set data [lassign $data element]
        return $element
    }

    method peek {} {
        return [lindex $data 0]
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
