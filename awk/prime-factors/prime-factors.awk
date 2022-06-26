#!/usr/bin/env gawk -f
@include "join"

{
    delete factors
    f = 2
    i = 0
    n = $1

    while (f <= sqrt(n)) {
        if (n % f == 0) {
            factors[++i] = f
            n /= f
        }
        else
            f += (f == 2 ? 1 : 2)
    }
    if (n > 1) factors[++i] = n

    print join(factors, 1, i)
}
