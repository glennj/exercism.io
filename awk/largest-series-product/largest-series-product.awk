#!/usr/bin/env gawk -f
@include "assert"

BEGIN { FS = "," }
{
    len = split($1, digits, //)
    assert($2 <= len, "span must not exceed string length")
    assert($2 >= 0, "span must not be negative")
    assert($0 ~ /^[[:digit:]]*,[[:digit:]]+$/, "input must only contain digits")

    lsp = product(digits, 1, $2)
    for (i = 2; i <= len - $2 + 1; i++)
        lsp = max(lsp, product(digits, i, i + $2 - 1))
    print lsp
}

function product(digits, start, end,    prod, i) {
    prod = 1
    for (i = start; i <= end; i++)
        prod *= digits[i]
    return prod
}
function max(a, b) {
    return a > b ? a : b
}
