#!/usr/bin/env gawk -f

@include "arrays"
@include "assert"
@include "join"

# command line variables: `ibase`, `obase`

BEGIN {
    assert(ibase >= 2, "Input base must be greater than one")
    assert(obase >= 2, "Output base must be greater than one")
}

function decimal_reducer(a, b) {
    assert(0 <= b && b < ibase, "Input digit out of range")
    return ibase * a + b
}

{ n = split($0, idigits) }
n == 0 { print ""; next }

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
