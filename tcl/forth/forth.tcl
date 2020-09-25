source ../lib/stack.tcl

proc evalForth {input} {
    [Forth new $input] evaluate
}

############################################################
oo::class create Forth {
    variable stack macros inputLines

    constructor {input} {
        set stack [Stack new]
        set macros [dict create]
        set inputLines [split [string toupper $input] \n]
    }

    method evaluate {} {
        foreach line $inputLines {
            my EvaluateLine [split $line]
        }
        return [lreverse [$stack contents]]
    }

    # reminder, by default only methods starting with
    # a lower-case letter are exported (public)

    method EvaluateLine {instructions} {
        while {[llength $instructions] > 0} {
            set instructions [lassign $instructions token]

            if {[dict exists $macros $token]} {
                set instructions [concat [dict get $macros $token] $instructions]

            } elseif {[string is integer -strict $token]} {
                $stack push $token

            } elseif {$token in {+ - * /}} {
                my ArithmeticOperation $token

            } elseif {$token in {DUP DROP SWAP OVER}} {
                my $token

            } elseif {$token eq ":"} {
                # TODO validate last token is a semicolon
                my AddMacro [lrange $instructions 0 end-1]
                set instructions [list]

            } else {
                error "undefined operation"
            }
        }
    }

    method ArithmeticOperation {op} {
        my MinStack 2
        set b [$stack pop]
        set a [$stack pop]
        $stack push [expr "$a $op $b"]
        return
    }

    method DUP {} {
        my MinStack 1
        $stack push [$stack peek]
        return
    }

    method DROP {} {
        my MinStack 1
        $stack pop
        return
    }

    method SWAP {} {
        my MinStack 2
        set b [$stack pop]
        set a [$stack pop]
        $stack push $b
        $stack push $a
        return
    }
 
    method OVER {} {
        my MinStack 2
        set b [$stack pop]
        set a [$stack peek]
        $stack push $b
        $stack push $a
        return
    }

    method MinStack {n} {
        if {$n > 0 && [$stack isEmpty]} {
            error "empty stack"
        } elseif {$n > 1 && [$stack size] == 1} {
            error "only one value on the stack"
        }
    }

    method AddMacro {instructions} {
        set instructions [lassign $instructions key]
        if {[string is integer -strict $key]} {
            error "illegal operation"
        }
        set value [list]
        foreach token $instructions {
            if {[dict exists $macros $token]} {
                lappend value {*}[dict get $macros $token]
            } else {
                lappend value $token
            }
        }
        dict set macros $key $value
    }
}
