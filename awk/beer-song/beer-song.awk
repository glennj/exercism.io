#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - verse
# - start
# - stop

BEGIN {
    Max = 99
    Beer = "of beer"
    Wall = "on the wall"

    if (typeof(verse) != "untyped") start = stop = verse
    for (verse = start; verse >= stop; verse--) printVerse(verse)
}

function printVerse(n,    b) {
    b = bottles(n)
    printf "%s %s %s, ", capitalize(b), Beer, Wall
    printf "%s %s.\n", b, Beer
    printf "%s, ", action(n)
    printf "%s %s %s.\n", bottles(n == 0 ? Max : n - 1), Beer, Wall
}

function bottles(n) {
    return sprintf("%s bottle%s", (n == 0 ? "no more" : n), (n == 1 ? "" : "s"))
}

function action(n) {
    if (n == 0) 
        return "Go to the store and buy some more"
    else
        return "Take " (n == 1 ? "it" : "one") " down and pass it around"
}

function capitalize(str) {
    return toupper(substr(str, 1, 1)) tolower(substr(str, 2))
}
