#!/usr/bin/env gawk -f

BEGIN {
    if (num % 3 == 0) drops = drops "Pling"
    if (num % 5 == 0) drops = drops "Plang"
    if (num % 7 == 0) drops = drops "Plong"

    if (drops)
        print (drops ? drops : num)
    else
        print num

    # or
    #   print (drops ? drops : num)
}
