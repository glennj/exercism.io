namespace eval Affine {
    namespace export encode decode
    namespace ensemble create

    variable alphabet {a b c d e f g h i j k l m n o p q r s t u v w x y z}
    variable m [llength $alphabet]

    proc encode {phrase a b} {
        variable m
        validate $a
        set coded [code $phrase x {expr {($a * $x + $b) % $m}}]
        return [join [regexp -all -inline {.{1,5}} $coded] " "]
    }

    proc decode {phrase a b} {
        variable m
        validate $a
        set a_inv [expr {mmi($a, $m)}]
        return [code $phrase y {expr {($a_inv * ($y - $b)) % $m}}]
    }

    proc validate {a} {
        variable m
        assert {gcd($a, $m) == 1} "a and m must be coprime."
    }

    proc code {phrase idxVar func} {
        variable alphabet
        variable m
        upvar 1 $idxVar i

        set map {}
        for {set i 0} {$i < $m} {incr i} {
            lappend map \
                [lindex $alphabet $i] \
                [lindex $alphabet [uplevel 1 $func]]
        }

        set input [string tolower [regsub -all {[^[:alnum:]]} $phrase ""]]
        return [string map $map $input]
    }
}

############################################################
proc ::tcl::mathfunc::gcd {a b} {
    if {$b == 0} {
        return $a
    }
    set procname [lindex [info level 0] 0]
    tailcall $procname $b [expr {$a % $b}]
}

# find `n` where `a*n mod m == 1`
proc ::tcl::mathfunc::mmi {a m} {
    for {set n 0} {$n < $m} {incr n} {
        if {$a * $n % $m == 1} {
            return $n
        }
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

############################################################
interp alias {} affine_encode {} Affine encode
interp alias {} affine_decode {} Affine decode
