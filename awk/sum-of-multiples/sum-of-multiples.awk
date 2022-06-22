#!/usr/bin/env gawk -f

{
    delete multiples 
    for (n = 1; n < limit; n++)
        for (i = 1; i <= NF; i++)
            if ($i > 0 && n % $i == 0)
                multiples[n] = 1
    sum = 0
    for (m in multiples)
        sum += m
    print sum
}
