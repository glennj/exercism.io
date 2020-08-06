source "tree.tcl"

oo::class create Zipper {
    variable tree
    variable focus
    variable parent

    constructor {aTree {aFocus {}} {aParent {}}} {
        set tree $aTree
        if {[my _isaTree $aFocus]} {
            set focus $aFocus
        } else {
            set focus $aTree
        }
        set parent $aParent
    }

    method _isaTree {item} {
        info object isa typeof $item Tree
    }

    method tree  {} {return $tree}
    method focus {} {return $focus}
    method up    {} {return $parent}

    method value {} {[my focus] value}

    method left {} {
        set left [[my focus] left]
        if {[my _isaTree $left]} {
            [self class] new [my tree] $left [self]
        }
    }

    method right {} {
        set right [[my focus] right]
        if {[my _isaTree $right]} {
            [self class] new [my tree] $right [self]
        }
    }

    method setValue {value} {[my focus] setValue $value}
    method setLeft  {value} {[my focus] setLeft  $value}
    method setRight {value} {[my focus] setRight $value}

    method equals {other} {
        expr {
            [[my tree]  equals [$other tree]] &&
            [[my focus] equals [$other focus]]
        }
    }
}
