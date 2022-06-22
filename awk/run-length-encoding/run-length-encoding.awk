#!/usr/bin/env gawk -f

# variable `type` initialized on the command line with `-v`

BEGIN {
    switch (type) {
        case "encode":
            # encoding, each character is field
            FS = ""
            break
        case "decode":
            # decoding, a field is some digits followed by a non-digit
            FPAT = "([[:digit:]]*)([^[:digit:]])"
            break
        default:
            exit 2
    }
}

{ @type() }

function encode(    prev, count, i) {
    prev = $1
    count = 1
    for (i = 2; i <= NF; i++) {
        if ($i != prev) {
            encode_output(count, prev)
            prev = $i
            count = 0
        }
        count++
    }
    encode_output(count, prev)
    print ""
}

function encode_output(count, prev) {
    printf "%s%s", (count == 1 ? "" : count), prev
}

function decode(    i, m) {
    for (i = 1; i <= NF; i++) {
        if (match($i, FPAT, m)) {
            if (m[1] == "")
                m[1] = 1
            for (j = 1; j <= m[1]; j++)
                printf "%s", m[2]
        }
    }
    print ""
}
