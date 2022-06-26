#!/usr/bin/env gawk -f

           { dist = hypot($1, $2) }
dist <=  1 { print 10; next }
dist <=  5 { print  5; next }
dist <= 10 { print  1; next }
           { print  0 }

function hypot(x, y) {
    return sqrt(x * x + y * y)
}