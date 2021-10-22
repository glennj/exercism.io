package provide structures::queue 0.1
package require structures::stack

# a Queue is not Sequenceable

oo::class create Queue {
    superclass Stack
    variable data

    method push {element} {
        set data [linsert $data[set data ""] 0 $element]
        return [self]
    }
}
