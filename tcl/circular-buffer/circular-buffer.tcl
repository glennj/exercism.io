oo::class create CircularBuffer {
    variable tape
    variable readPtr writePtr
    variable NULL

    constructor {cap} {
        set NULL \x0
        set tape [lrepeat $cap $NULL]
        my clear
    }

    method empty? {} {
        expr {[lindex $tape $readPtr] eq $NULL}
    }

    method full? {} {
        expr {[lindex $tape $writePtr] ne $NULL}
    }

    method read {} {
        if {[my empty?]} {
            error "buffer is empty"
        }
        set value [lindex $tape $readPtr]
        lset tape $readPtr $NULL
        my increment readPtr
        return $value
    }

    method write {value} {
        if {[my full?]} {
            error "buffer is full"
        }
        lset tape $writePtr $value
        my increment writePtr
        return
    }

    method increment {ptrName} {
        upvar 1 $ptrName ptr
        # implement the circularity: wrap to zero when at end of tape
        set ptr [expr {(1 + $ptr) % [llength $tape]}]
    }
    unexport increment ;# private

    method overwrite {value} {
        if {[my full?]} {
            # discard oldest value
            my read
        }
        my write $value
    }

    method clear {} {
        set tape [lmap cell $tape {set NULL}]
        set readPtr 0
        set writePtr 0
        return
    }
}
