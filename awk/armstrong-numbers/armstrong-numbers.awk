#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - num

BEGIN {
    print isArmstrong(num) ? "true" : "false"
    exit
}

function isArmstrong(num,    n, digits, sum) {
    n = split(num, digits, "")
    for (i in digits)
        sum += digits[i] ^ n
    return sum == num
}
