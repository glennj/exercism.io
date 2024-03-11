oo::class create CircularBuffer {
    variable tape
    variable capacity
    variable count
    variable readPtr writePtr

    constructor {cap} {
        set tape [lrepeat $cap ""]
        set capacity $cap
        my clear
    }

    method clear {} {
        set count 0
        set readPtr 0
        set writePtr 0
        return
    }

    method empty? {} {
        expr {$count == 0}
    }

    method full? {} {
        expr {$count == $capacity}
    }

    method read {} {
        if {[my empty?]} {
            error "buffer is empty"
        }
        set value [lindex $tape $readPtr]
        my increment readPtr
        incr count -1
        return $value
    }

    method write {value} {
        if {[my full?]} {
            error "buffer is full"
        }
        lset tape $writePtr $value
        my increment writePtr
        incr count
        return
    }

    method increment {ptrName} {
        upvar 1 $ptrName ptr
        # implement the circularity: wrap to zero when at end of tape
        set ptr [expr {(1 + $ptr) % $capacity}]
    }
    unexport increment ;# private

    method overwrite {value} {
        if {[my full?]} {
            # discard oldest value
            my read
        }
        my write $value
    }
}
