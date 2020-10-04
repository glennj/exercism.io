package provide set 0.1

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

    method contains {element} {
        dict exists $data $element
    }

    method remove {args} {
        foreach element $args {
            dict unset data $element 
        }
    }

    method empty {} {
        set data [dict create]
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

    method clone {} {
        set new [Set new]
        $new add {*}[my toList]
        return $new
    }
}
