proc ::tcl::mathfunc::gcd {a b} {
    if {$b == 0} {
        return $a
    }
    set procname [lindex [info level 0] 0]
    tailcall $procname $b [expr {$a % $b}]
}
