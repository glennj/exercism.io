package provide structures::set 0.1
# TODO documentation!

package require Tcl 8.6-
set {IsTcl8.7+} [package vsatisfies [package provide Tcl] 8.7-]

package require structures::sequenceable

oo::class create Set {
    variable data

    constructor {} {
        set data [dict create] 
    }

    # private method used by Sequenceable
    method Data {return [dict keys $data]}

    method add {args} {
        foreach item $args {
            dict set data $item "" 
        }
        return [self]
    }

    # override these from Sequenceable: dict operations more efficient
    method contains {item} {
        dict exists $data $item
    }

    method containsAll {items} {
        foreach item $items {
            if {![my contains $item]} then {return false}
        }
        return true
    }

    method remove {args} {
        foreach item $args {
            dict unset data $item 
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

    forward toList  my Data

    method clone {} {
        set new [[self class] new]
        $new add {*}[my toList]
        return $new
    }
}
