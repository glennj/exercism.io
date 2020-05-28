proc spiralMatrix {n} {
    if {$n == 0} then return

    set matrix [lrepeat $n [lrepeat $n NaN]]

    set x 0
    set y 0

    cycle nextDelta {0 1} {1 0} {0 -1} {-1 0}
    lassign [nextDelta] dx dy

    for {set i 1} {$i <= $n**2} {incr i} {
        lset matrix $x $y $i

        if { $x + $dx < 0 || $x + $dx == $n ||
             $y + $dy < 0 || $y + $dy == $n ||
             [string is integer [lindex $matrix $x+$dx $y+$dy]]
        } then {
            lassign [nextDelta] dx dy
        }
        incr x $dx
        incr y $dy
    }

    rename nextDelta ""
    return $matrix
}

# Inspired by ruby
# https://ruby-doc.org/core-2.6.3/Array.html#method-i-cycle
#
proc cycle {cmdName args} {
    coroutine $cmdName apply {{elements} {
        yield [info coroutine]
        set idx 0
        set len [llength $elements]
        while {1} {
            yield [lindex $elements $idx]
            set idx [expr {($idx + 1) % $len}]
        }
    }} $args
}

# not required by the test suite:
#   % printSpiralMatrix 11
#     1   2   3   4   5   6   7   8   9  10  11
#    40  41  42  43  44  45  46  47  48  49  12
#    39  72  73  74  75  76  77  78  79  50  13
#    38  71  96  97  98  99 100 101  80  51  14
#    37  70  95 112 113 114 115 102  81  52  15
#    36  69  94 111 120 121 116 103  82  53  16
#    35  68  93 110 119 118 117 104  83  54  17
#    34  67  92 109 108 107 106 105  84  55  18
#    33  66  91  90  89  88  87  86  85  56  19
#    32  65  64  63  62  61  60  59  58  57  20
#    31  30  29  28  27  26  25  24  23  22  21
#
proc printSpiralMatrix {n} {
    set wid [expr {1 + int(log10($n**2))}]
    foreach row [spiralMatrix $n] {
        puts [join [lmap x $row {format {%*d} $wid $x}]]
    }
}
