#!/usr/bin/env gawk -f

BEGIN {
    gigasecond = 1e9
    FS = "[-T:]"
}

{ 
    # initialize the hour/minute/second fields if they are absent
    $4 += 0; $5 += 0; $6 += 0

    # because OFS is a space (the default), $0 is now a space-separated string
    epoch = mktime($0, 1)

    print strftime("%FT%T", epoch + gigasecond, 1)
}
