#!/usr/bin/env ksh

# https://blog.fpmurphy.com/2013/08/custom-arithmetic-functions-in-korn-shell-93.html

# some convenience ksh math functions

function .sh.math.max a b {
    ((.sh.value = a > b ? a : b))
}

function .sh.math.min a b {
    ((.sh.value = a > b ? b : a))
}

function .sh.math.gcd a b {
    integer _a=$((abs(a))) _b=$((abs(b)))
    ((.sh.value = _b > 0 ? gcd(_b, (_a % _b)) : _a))
}
