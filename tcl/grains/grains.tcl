namespace eval grains {
    namespace export square total
    namespace ensemble create

    proc square {square} {
        assert {1 <= $square && $square <= 64} \
            "square must be between 1 and 64"
        expr {2 ** ($square - 1)}
    }

    proc total {} {
        expr {2 ** 64 - 1}
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
