############################################################
oo::class create Value {
    variable value
    method value {} {return $value}
}

############################################################
oo::class create Cell {
    mixin Value

    variable listeners

    constructor {} {
        set listeners {}
    }

    method addListener {cell} {
        lappend listeners $cell
    }

    method updateListeners {} {
        foreach cell $listeners {
            $cell update
        }
    }

    method fireListenerCallbacks {} {
        foreach cell $listeners {
            $cell fireCallbacks
        }
    }
}

############################################################
oo::class create InputCell {
    superclass Cell

    variable value
    variable listeners

    constructor {aValue} {
        next
        set value $aValue
    }

    method setValue {aValue} {
        set value $aValue
        my updateListeners
        my fireListenerCallbacks
    }
}

############################################################
oo::class create ComputeCell {
    superclass Cell

    variable value
    variable listeners

    variable inputCells
    variable previousValue
    variable computeFunc
    variable callbacks

    constructor {cells func} {
        next
        set inputCells $cells
        set computeFunc $func

        foreach cell $cells {
            $cell addListener [self]
        }
        my update
        set previousValue $value
        set callbacks {}
    }

    method update {} {
        set value [apply $computeFunc [lmap cell $inputCells {$cell value}]]
        my updateListeners
    }

    method addCallback {func} {
        set cb [Callback new $func [self]]
        lappend callbacks $cb
        return $cb
    }

    method fireCallbacks {} {
        if {$value != $previousValue} {
            set previousValue $value
            foreach cb $callbacks {
                $cb fire
            }
            my fireListenerCallbacks
        }
    }

    method removeCallback {callback} {
        set idx [lsearch -exact $callbacks $callback]
        if {$idx != -1} {
            set callbacks [lreplace $callbacks $idx $idx]
        }
    }
}

############################################################
oo::class create Callback {
    mixin Value

    variable value
    variable computeCell
    variable callbackFunc

    constructor {func cell} {
        set callbackFunc $func
        set computeCell $cell
        set value ""
    }

    method fire {} {
        set value [apply $callbackFunc $computeCell]
    }
}
