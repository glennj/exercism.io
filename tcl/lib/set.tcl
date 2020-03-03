oo::class create Set {
    variable data

    constructor {} {
        set data [dict create] 
    }

    method add {args} {
        foreach element $args {
            dict set data $element "" 
        }
    }

    method remove {args} {
        foreach element $args {
            dict unset data $element 
        }
    }

    method size {} {
        dict size $data 
    }

    method isEmpty {} {
        expr {[my size] == 0} 
    }

    method toList {} {
        dict keys $data 
    }
}
