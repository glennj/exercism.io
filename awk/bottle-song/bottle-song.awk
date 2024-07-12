# These variables are initialized on the command line (using '-v'):
# - startBottles
# - takeDown

BEGIN {
    split("One Two Three Four Five Six Seven Eight Nine Ten", number, " ")
    number[0] = "No"

    template = "%s%s,\n%s,\nAnd if one green bottle should accidentally fall,\nThere'll be %s.\n"

    sep = ""
    for (i = startBottles; i > startBottles - takeDown; i--) {
        printf template, sep, bottles(i, 0), bottles(i, 0), bottles(i - 1, 1)
        sep = "\n"
    }
}

function bottles(n, lowered,    b) {
    b = number[n] " green bottle" (n == 1 ? "" : "s") " hanging on the wall"
    return lowered ? tolower(b) : b
}
