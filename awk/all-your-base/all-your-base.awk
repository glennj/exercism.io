#!/usr/bin/env gawk -f

@include "arrays"
#@include "assert"
@include "join"

# command line variables: `ibase`, `obase`

function assert(condition, msg) {
    if (!condition) {
        print msg > "/dev/stderr"
        exit(1)
    }
}

BEGIN {
    assert(ibase >= 2, "input base must be >= 2")
    assert(obase >= 2, "output base must be >= 2")
}

function decimal_reducer(a, b) {
    assert(0 <= b && b < ibase, "all digits must satisfy 0 <= d < input base")
    return ibase * a + b
}

{ n = split($0, idigits) }
n == 0 { print 0; next }

{ decimal = arrays::reduce(idigits, "decimal_reducer", 0) }
decimal == 0 { print 0; next }

{
    arrays::init(odigits)
    while (decimal > 0) {
        arrays::push(odigits, decimal % obase)
        decimal = int(decimal / obase)
    }

    arrays::reverse(odigits)
    print join(odigits, 1, length(odigits))
}
