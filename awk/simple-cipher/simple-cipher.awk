# These variables are initialized on the command line (using '-v'):
# - type
# - key

@include "ord"
@include "die"
@include "math"

BEGIN {
    lower_a = ord("a")

    if (typeof(key) == "untyped") {
        # generate a random key, print it, and exit
        for (i = 1; i <= 100; i++) 
            key = key randomLetter()
        print key
        exit
    }
}

# key must be all lowercase
key !~ /^[[:lower:]]+$/ { die("invalid key") }

function randomLetter() { return chr(lower_a + int(rand() * 26)) }

function encipher(text, dir,    coded, i, c, k, x) {
    while (length(key) < length(text))
        key = key key

    for (i = 1; i <= length(text); i++) {
        c = ord(substr(text, i, 1))
        k = ord(substr(key, i, 1))
        x = math::floorMod((c - lower_a) + dir * (k - lower_a), 26)
        coded = coded chr(lower_a + x)
    }

    return coded
}

type == "encode" { print encipher(tolower($0), +1); next }
type == "decode" { print encipher(tolower($0), -1); next }
