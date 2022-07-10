#!/usr/bin/env gawk -f

BEGIN {
    gigasecond = 1e9
    FS = "[-T:]"
}

NF == 3 { datespec = $1 " " $2 " " $3 " 0 0 0" }
NF == 6 { datespec = $1 " " $2 " " $3 " " $4 " " $5 " " $6 }
{
    epoch = mktime(datespec, 1)
    print strftime("%FT%T", epoch + gigasecond, 1)
}
