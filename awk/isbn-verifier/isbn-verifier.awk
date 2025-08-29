#!/usr/bin/env gawk -f

BEGIN {
    isbn10 = @/^([0-9]{9})([0-9X])$/
}

{
    gsub(/-/, "", $1)

    sum = 0
    if ( !match($1, isbn10, m) ) {
        sum = -1
    } else {
        for (i = 1; i <= 9; i++) {
            sum += (11 - i) * (0 + substr(m[1], i, 1))
        }
        sum += m[2] == "X" ? 10 : (0 + m[2])
    }

    print sum % 11 == 0 ? "true" : "false"
}
