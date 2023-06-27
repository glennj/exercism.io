namespace eval ::seq {
    namespace export seq

    # Generate a sequence of numbers
    #
    # seq -to 10                 ;# => 0 1 2 3 4 5 6 7 8 9 10
    # seq -to 10 -from 14 -by -2 ;# => 14 12 10
    # seq -from 1 -to 5 -by -2   ;# error
    # seq -from 1 -to 5 -by 0    ;# error
    #
    proc seq {args} {
        set len [llength $args]
        if {$len == 0 || ($len % 2) == 1} {
            error {usage: seq [-from A] [-to B] [-by C]}
        }

        array set arg [list -from 0 -to 0 -by 1 {*}$args]
        set from $arg(-from)
        set to   $arg(-to)
        set step $arg(-by)

        if {$step == 0
            || ($step > 0 && $from > $to)
            || ($step < 0 && $from < $to)
        } {
            error "invalid step"
        }

        if {$step > 0} {
            proc ::tcl::mathfunc::seq_cmp_ {a b} {expr $a <= $b}
        } else {
            proc ::tcl::mathfunc::seq_cmp_ {a b} {expr $a >= $b}
        }

        set seq {}
        for {} {seq_cmp_($from, $to)} {incr from $step} {
            lappend seq $from
        }

        rename ::tcl::mathfunc::seq_cmp_ ""
        return $seq
    }
}
