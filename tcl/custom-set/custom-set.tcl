oo::class create Set {
    variable data

    constructor {{elements {}}} {
        set data [dict create]
        my addAll $elements
    }

    method toList   {}  {dict keys $data}
    method size     {}  {dict size $data}
    method isEmpty  {}  {expr {[my size] == 0}}
    method contains {e} {dict exists $data $e}

    method add {elem} {
        dict set data $elem ""
        return [self]
    }

    method addAll {elements} {
        foreach elem $elements {
            my add $elem
        }
        return [self]
    }

    method equals {other} {
        expr {[my size] == [$other size] && [my subsetOf $other]}
    }

    method foreach {varName body} {
        upvar 1 $varName var
        dict for {var _} $data {uplevel 1 $body}
    }

    method subsetOf {other} {
        # This can be implemented with
        #    expr {[my size] == [[my intersection $other] size]}
        # but that must examine every element in the set.
        # This short-circuits:

        set result true
        my foreach elem {
            if {![$other contains $elem]} {
                set result false
                break
            }
        }
        return $result
    }

    method disjoint {other} {
        # Similar to above
        #    [my intersection $other] isEmpty

        set result true
        my foreach elem {
            if {[$other contains $elem]} {
                set result false
                break
            }
        }
        return $result

    }

    method partition {other} {
        set intersection [[self class] new]
        set difference   [[self class] new]
        my foreach elem {
            if {[$other contains $elem]} {
                $intersection add $elem
            } else {
                $difference add $elem
            }
        }
        return [list $intersection $difference]
    }

    method intersection {other} {
        lindex [my partition $other] 0
    }

    method difference {other} {
        lindex [my partition $other] 1
    }

    method union {other} {
        set result [[self class] new]
        my     foreach elem {$result add $elem}
        $other foreach elem {$result add $elem}
        return $result
    }
}
