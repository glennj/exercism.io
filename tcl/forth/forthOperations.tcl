oo::class create ForthToken {
    variable stack
    constructor {_stack} {
        set stack $_stack
    }
    method requireStackSize {n} {
        if {$n > 0 && [$stack isEmpty]} {
            error "empty stack"
        } elseif {$n > 1 && [$stack size] == 1} {
            error "only one value on the stack"
        }
    }
    method execute {} {
        error "subclass responsibility"
    }
}

oo::class create UserDefined {
    variable stack
    variable definition
    variable opname
    constructor {_stack operations tokens} {
        set stack $_stack
        set tokens [lassign $tokens opname]
        if {[string is integer -strict $opname]} {
            error "illegal operation"
        }
        set definition [list]
        foreach token $tokens {
            if {[string is integer -strict $token]} {
                lappend definition [Number new $stack $token]
            } elseif {[dict exists $operations $token]} {
                lappend definition {*}[dict get $operations $token]
            } else {
                lappend definition $token
            }
        }
    }
    method opname {} {return $opname}
    method execute {} {
        foreach token $definition {
            $token execute
        }
    }
}

oo::class create Number {
    variable value
    variable stack
    constructor {_stack _value} {
        set stack $_stack
        set value $_value
    }
    method execute {} {
        $stack push $value
    }
}

oo::class create Add {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        set b [$stack pop]
        $stack push [expr {$b + $a}]
    }
}

oo::class create Subtract {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        set b [$stack pop]
        $stack push [expr {$b - $a}]
    }
}

oo::class create Multiply {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        set b [$stack pop]
        $stack push [expr {$b * $a}]
    }
}

oo::class create Divide {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        if {$a == 0} then {error "divide by zero"}
        set b [$stack pop]
        $stack push [expr {$b / $a}]
    }
}

oo::class create Drop {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 1
        $stack pop
        return
    }
}

oo::class create Dup {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 1
        $stack push [$stack peek]
        return
    }
}

oo::class create Over {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        set b [$stack peek]
        [$stack push $a] push $b
        return
    }
}

oo::class create Swap {
    superclass ForthToken
    variable stack
    method execute {} {
        my requireStackSize 2
        set a [$stack pop]
        set b [$stack pop]
        [$stack push $a] push $b
        return
    }
}
