# Using the Binary numeral system (base 2) from Wikipedia
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
#
proc squareRoot {n} {
    # find b, the greatest power of 4 <= n
    set b [expr {4 ** int(log($n) / log(4))}]
    set x 0

    while {$b != 0} {
        if {$n >= $x + $b} {
            set n [expr {$n - $x - $b}]
            set x [expr {$x / 2 + $b}]
        } else {
            set x [expr {$x / 2}]
        }
        set b [expr {$b / 4}]
    }

    return $x
}
