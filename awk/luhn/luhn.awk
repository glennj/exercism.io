#! awk

BEGIN {
    FS = ""
    # not doubled
    D[0,0] = 0; D[0,1] = 1; D[0,2] = 2; D[0,3] = 3; D[0,4] = 4
    D[0,5] = 5; D[0,6] = 6; D[0,7] = 7; D[0,8] = 8; D[0,9] = 9
    # doubled
    D[1,0] = 0; D[1,1] = 2; D[1,2] = 4; D[1,3] = 6; D[1,4] = 8
    D[1,5] = 1; D[1,6] = 3; D[1,7] = 5; D[1,8] = 7; D[1,9] = 9
}

function is_luhn(    sum, dbl, i) {
    sum = dbl = 0
    for (i = NF; i > 0; i--) {
        sum += D[dbl, $i]
        dbl = !dbl
    }
    return sum % 10 == 0
}

{ gsub(/[[:blank:]]/, "") }

/[^[:digit:]]/ || NF <= 1 || ! is_luhn() { print "false";  next }

{ print "true" }
