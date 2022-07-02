#!/usr/bin/env gawk -f

@include "assert"

# These variables are initialized on the command line (using '-v'):
# - action

BEGIN {
    MSB = 128    # 0b10000000
    MASK = 127   # 0b01111111
    BITS = 7
    assert(action == "encode" || action == "decode", "unknown action")
}

action == "encode" {
    delete encoded
    n = 0
    for (i = 1; i <= NF; i++)
        encoded[++n] = encode(hex2dec($i))
    print join(encoded)
}

action == "decode" {
    delete quantities
    n = 0
    quantity = 0
    for (i = 1; i <= NF; i++) {
        dec = hex2dec($i)
        quantity = or(lshift(quantity, BITS), and(dec, MASK))
        has_msb = and(dec, MSB)
        if (has_msb == 0) {
            quantities[++n] = dec2hex(quantity)
            quantity = 0
        }
    }
    assert(has_msb == 0, "incomplete byte sequence")
    print join(quantities)
}

############################################################
function encode(number,    result, n, i, msb, out) {
    if (number == 0) 
        result[++n] = dec2hex(number)
    else {
        msb = 0
        while (number > 0) {
            quantity = or(and(number, MASK), msb)
            number = rshift(number, BITS)
            msb = MSB
            result[++n] = dec2hex(quantity)
        }
    }
    reverse(result)
    return join(result)
}

############################################################
function hex2dec(h) { return strtonum("0X" h) }
function dec2hex(d) { return sprintf("%02X", d) }

function reverse(array,    i, j, tmp) {
    i = 1; j = length(array)
    while (i < j) {
        tmp = array[i]
        array[i] = array[j]
        array[j] = tmp
        i++; j--
    }
}

function join(array,    out, i) {
    out = array[1]
    for (i = 2; i <= length(array); i++)
        out = out " " array[i]
    return out
}
