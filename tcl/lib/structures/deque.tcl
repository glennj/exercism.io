package provide structures::deque 0.1
package require structures::stack

# a Deque is not Sequenceable

oo::class create Deque {
    superclass Stack
    variable data

    method shift {element} {
        set data [linsert $data[set data ""] 0 $element]
        return [self]
    }

    method unshift {} {
        set data [lassign $data[set data ""] element]
        return $element
    }

    method peekHead {} {
        return [lindex $data 0]
    }
    forward peekTail  my peek
}
