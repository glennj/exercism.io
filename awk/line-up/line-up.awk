#function ordinal(n,    suffix, ones, tens) {
#    suffix = "th"
#    ones = n % 10
#    tens = n % 100
#    if (ones == 1 && tens != 11) suffix = "st"
#    if (ones == 2 && tens != 12) suffix = "nd"
#    if (ones == 3 && tens != 13) suffix = "rd"
#    return n suffix
#}
@include "arrays"

BEGIN {
    for (i = 0; i <= 90; i += 10) {
        if (i == 10) continue
        suffix[i + 1] = "st"
        suffix[i + 2] = "nd"
        suffix[i + 3] = "rd"
    }
    #arrays::pprint(suffix)
}

function ordinal(n) {
    return n ((n % 100) in suffix ? suffix[n % 100] : "th")
}
NF {
    printf("%s, you are the %s customer we serve today. Thank you!\n", $1, ordinal($2))
}
