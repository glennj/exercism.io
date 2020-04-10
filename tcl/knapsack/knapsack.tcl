# Translating the pseudocode from https://en.wikipedia.org/wiki/Knapsack_problem
#
#  Assume w_{1}, w_{2}, ..., w_{n}, W are strictly positive integers. 
#  Define m[i,w] to be the maximum value that can be attained with weight
#  less than or equal to w using items up to i (first i items).
# 
#  We can define m[i,w] recursively as follows: 
#  * m[0,w] = 0
#  * m[i,w] = m[i-1,w] if w_{i} > w (the new item is more than the current weight limit)
#  * m[i,w] = max(m[i-1,w], m[i-1,w-w_{i}]+v_{i}) if w_{i} <= w.
#
# The solution can then be found by calculating m[n,W]. 

proc maximumValue {maxWeight items} {
    set n [llength $items]
    array set m {}

    # max value for each weight for no items
    for {set w 0} {$w <= $maxWeight} {incr w} {
        set m(0,$w) 0
    }

    for {set i 1} {$i <= $n} {incr i} {
        # Tcl allows any character in a variable name,
        # so long as you use braces around the name
        set {i-1} [expr {$i - 1}]

        set item [lindex $items ${i-1}]
        set wi [dict get $item weight]
        set vi [dict get $item value]

        for {set w 0} {$w <= $maxWeight} {incr w} {
            set _w [expr {$w - $wi}]

            set m($i,$w) [expr {
                $wi > $w
                    ? $m(${i-1},$w)
                    : max($m(${i-1},$w), $vi + $m(${i-1},$_w))
            }]
        }
    }
    return $m($n,$maxWeight)
}
