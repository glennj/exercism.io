oo::class create ComplexNumber {
    variable a  ;# real part
    variable b  ;# imaginary part

    constructor {real {imag 0}} {
        if {[string match *i* $real]} {
            lassign [split $real i] a b
        } else {
            set a $real
            set b $imag
        }
    }

    method real     {} { return $a }
    method imag     {} { return $b }
    method toList   {} { list $a $b }

    # some precision truncation to represent something like
    # "-1.0i1.2246467991473532e-16" as "-1.0i0.0"
    method toString {{precision 10}} {
        set rounded {{n p} {expr {[format "%.*f" $p $n] + 0}}}
        if {[string is integer $a]} {
            set aa $a
        } else {
            set aa [apply $rounded $a $precision]
        }
        if {[string is integer $b]} {
            set bb $b
        } else {
            set bb [apply $rounded $b $precision]
        }
        string cat $aa "i" $bb
    }

    method add {other} {
        [self class] new [expr {$a + [$other real]}] \
                         [expr {$b + [$other imag]}]
    }

    method sub {other} {
        [self class] new [expr {$a - [$other real]}] \
                         [expr {$b - [$other imag]}]
    }

    method mul {other} {
        set c [$other real]
        set d [$other imag]
        [self class] new [expr {$a * $c - $b * $d}] \
                         [expr {$b * $c + $a * $d}]
    }

    method div {other} {
        set c [$other real]
        set d [$other imag]
        set denom [expr {$c**2 + $d**2}]
        [self class] new [expr {double($a * $c + $b * $d) / $denom}] \
                         [expr {double($b * $c - $a * $d) / $denom}]
    }

    forward  +  my add
    forward  -  my sub
    forward  *  my mul
    forward  /  my div
    export + - * /

    method abs {} {
        expr {hypot($a, $b)}
    }

    method conj {} {
        [self class] new $a [expr {-1 * $b}]
    }

    method exp {} {
        set u [[self class] new [expr {exp(1) ** $a}] 0]
        set v [[self class] new [expr {cos($b)}] [expr {sin($b)}]]
        return [$u mul $v]
    }
}

############################################################
# In addition to passing the tests for the exercise, add some usability
# improvements:
#
# Define some math functions for `expr`
#   cadd(first, second)
#   csub(first, second)
#   cmul(first, second)
#   cdiv(first, second)
#   cabs(first)
#   cconj(first)
#   cexp(first)
#
# Where "first" and "second" are strings of the form: _real_ "i" _imag_
# e.g. "10i20"

proc ::tcl::mathfunc::cadd {u v} {[[ComplexNumber new $u] + [ComplexNumber new $v]] toString}
proc ::tcl::mathfunc::csub {u v} {[[ComplexNumber new $u] - [ComplexNumber new $v]] toString}
proc ::tcl::mathfunc::cmul {u v} {[[ComplexNumber new $u] * [ComplexNumber new $v]] toString}
proc ::tcl::mathfunc::cdiv {u v} {[[ComplexNumber new $u] / [ComplexNumber new $v]] toString}

proc ::tcl::mathfunc::cabs  {cn} {[ComplexNumber new $cn] abs}
proc ::tcl::mathfunc::cconj {cn} {[[ComplexNumber new $cn] conj] toString}
proc ::tcl::mathfunc::cexp  {cn} {[[ComplexNumber new $cn] exp] toString}
