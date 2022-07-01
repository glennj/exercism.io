#!/usr/bin/env gawk -f

@include "math"
@include "die"

BEGIN {
    FS = "|"
    m = split("abcdefghijklmnopqrstuvwxyz", alphabet, "")
}

{
    a = $2; b = $3
    assert(math::gcd(a, m) == 1, "a and m must be coprime.")

    create_map($1, map)

    encrypted = ""
    n = split(prepare_input($4), chars, "")
    for (i = 1; i <= n; i++)
        encrypted = encrypted map[chars[i]]

    print ($1 == "encode" ? group5(encrypted) : encrypted)
}

function prepare_input(text) {
    gsub(/[^[:alnum:]]/, "", text)
    return tolower(text)
}

function create_map(action, map,    x, e, from, to) {
    for (x = 0; x < m; x++) {
        e = (a * x + b) % m
        from = alphabet[x + 1]
        to = alphabet[e + 1]

        if (action == "encode")
            map[from] = to
        else
            map[to] = from
    }
    # add the digits
    for (x = 0; x <= 9; x++) map[x] = x
}

function group5(input,    output, i) {
    output = substr(input, 1, 5)
    for (i = 6; i <= length(input); i += 5)
        output = output " " substr(input, i, 5)
    return output
}
