oo::class create BinarySearchTree {
    variable data
    variable left
    variable right

    # No constructor, so the instance variables do not exist 
    # until they are assigned.

    # Verify if an instance variable has been initialized
    method has {instVarName} {
        # Note level is zero: this is aliasing a method local variable
        # to an instance variable
        upvar 0 $instVarName instVar
        return [info exists instVar]
    }

    method data {} {
        return [expr {[my has data] ? $data : ""}]
    }

    method insert {value} {
        if {![my has data]} {
            set data $value
        } elseif {$value <= $data} {
            tailcall my insertInto left $value
        } else {
            tailcall my insertInto right $value
        }
    }

    method insertInto {sideVar value} {
        upvar 0 $sideVar side           
        if {![my has $sideVar]} {
            set side [[self class] new]
        }
        tailcall $side insert $value
    }
    unexport insertInto ;# private

    method toDict {} {
        set result {}
        dict set result data  [my data]
        dict set result left  [expr {[my has left]  ? [$left toDict] : ""}]
        dict set result right [expr {[my has right] ? [$right toDict] : ""}]
        return $result
    }

    # This is like a `foreach` method: walk the tree in order,
    # and execute some block of code for each node in the tree.
    #
    method inorder {varName body {level 1}} {
        if {[my has left]} {
            $left inorder $varName $body [expr {$level + 1}]
        }

        uplevel $level [list set $varName [self]]
        uplevel $level $body

        if {[my has right]} {
            $right inorder $varName $body [expr {$level + 1}]
        }
    }

    # Walk the tree in order, and return a list of the result
    # of executing some code for each node.
    #
    method map {varName body} {
        set result {}
        upvar 1 $varName node
        my inorder node {lappend result [uplevel 1 $body]}
        return $result
    }

    # Now, we get a couple of methods very simply.

    # A list of the node data values
    forward sorted  my map node {$node data}

    # A flattened list of the nodes in this tree
    forward toList  my map node {set node}
}
