oo::class create Queen {
    variable x
    variable y

    constructor {row col} {
        my Validate $row "row"
        my Validate $col "column"

        set x $row
        set y $col
    }

    method Validate {value what} {
        assert {$value >= 0} "$what not positive"
        assert {$value <  8} "$what not on board"
    }

    method row {} {return $x}
    method col {} {return $y}

    method canAttack {other} {
        assert {[info object isa typeof $other [self class]]} "not a Queen"

        set dx [expr {abs([my row] - [$other row])}]
        set dy [expr {abs([my col] - [$other col])}]

        assert {!($dx == 0 && $dy == 0)} "same position"
        expr {$dx == 0 || $dy == 0 || $dx == $dy}
    }
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
