#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# -direction

@load "ordchr"

BEGIN {
    a = ord("a")
    for (i = 1; i <= 26; i++) translation[chr(a + i - 1)] = chr(a + 26 - i)
    for (i = 0; i <=  9; i++) translation[i] = i
    FPAT = "[[:alnum:]]"
}

{
    ciphertext = ""
    for (i = 1; i <= NF; i++)
        ciphertext = ciphertext translation[tolower($i)]

    if (direction == "encode") {
        ciphertext = gensub(/.{1,5}/, " &", "g", ciphertext)
        sub(/^ /, "", ciphertext)
    }

    print ciphertext
}
