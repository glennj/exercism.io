#!/usr/bin/env gawk -f

@include "arrays"

BEGIN {
    FS = OFS = ","
    size = split("wink,double blink,close your eyes,jump", actions)
}

{
    code = $0
    arrays::init(shake)
    for (i = 1; i <= size; i++) 
        if (and(code, lshift(1, i - 1)))
            arrays::push(shake, actions[i])
    if (and(1, rshift(code, size)))
        arrays::reverse(shake)
    print arrays::join(shake)
}
