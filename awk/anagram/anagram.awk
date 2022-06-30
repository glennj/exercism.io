#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - key

BEGIN {
    PROCINFO["sorted_in"] = "@val_str_asc"
    key_coded = coder(key)
    key_lower = tolower(key)
}

key_lower != tolower($1) && key_coded == coder($1)

function coder(str,    chars, result) {
    split(tolower(str), chars, //)
    for (i in chars) result = result chars[i]
    return result
}
