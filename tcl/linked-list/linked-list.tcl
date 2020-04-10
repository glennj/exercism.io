oo::class create Node {
    variable next
    variable prev
    variable value
    
    constructor {data} {
        set next ""
        set prev ""
        set value $data
    }

    method value {} {return $value}

    method next {} {return $next}
    method prev {} {return $prev}

    method setNext {node} {set next $node}
    method setPrev {node} {set prev $node}
}


oo::class create LinkedList {
    variable head
    variable tail

    constructor {} {
        set head ""
        set tail ""
    }

    # add a new node to the tail of the linked list
    method push {data} {
        set node [Node new $data]
        if {$tail eq ""} {
            set head $node
        } else {
            $node setPrev $tail
            $tail setNext $node
        }
        set tail $node
    }

    # add a new node at the head of the linked list
    method unshift {data} {
        set node [Node new $data]
        if {$head eq ""} {
            set tail $node
        } else {
            $node setNext $head
            $head setPrev $node
        }
        set head $node
    }

    # remove a node from the tail of the linked list
    # and return the value
    method pop {} {
        if {$tail eq ""} then return
        set node $tail
        if {$head eq $tail} {
            set head [set tail ""]
        } else {
            set tail [$node prev]
            $tail setNext ""
        }
        return [$node value]
    }

    # remove a node from the head of the linked list
    # and return the value
    method shift {} {
        if {$head eq ""} then return
        set node $head
        if {$head eq $tail} {
            set head [set tail ""]
        } else {
            set head [$node next]
            $head setPrev ""
        }
        return [$node value]
    }

    # walk the list, and execute some code with each value
    method for {varname script} {
        upvar 1 $varname value
        for {set node $head} {$node ne ""} {set node [$node next]} {
            set value [$node value]
            uplevel 1 $script
        }
    }

    method length {} {
        set length 0
        my for _ {incr length}
        return $length
    }

    # returns a new linked list
    method reversed {} {
        set rev [[self class] new]
        my for value {$rev unshift $value}
        return $rev
    }
}
