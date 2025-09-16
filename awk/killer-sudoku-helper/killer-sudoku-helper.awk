############################################################
# This is a translation of my Tcl solution.
#     https://exercism.org/tracks/tcl/exercises/killer-sudoku-helper/solutions/glennj
# AWK's array handling makes this much more awkward.
############################################################

@include "join"

{
    print combinations($1, $2, concatenate(3, NF))
}

# A recursive function.
# Returns the valid combinations as a string, newline separated,
# with the combinations sorted.
#
# ex: combinations(22, 3, "")  => "5 8 9\n6 7 9"
#     combinations(22, 3, "5") => "6 7 9"
#
function combinations(sum, size, exclusions,
                      i, j, results, subcombinations, scs, n) {

    if (size == 1) {
        if (1 <= sum && sum <= 9 && !isExcluded(exclusions, sum)) {
            return sum
        }
    } else {
        # initialize an array
        delete results; results[1] = ""; delete results[1]

        for (i = 1; i <= 9; i++) {
            if (!isExcluded(exclusions, i) && sum - i > 0) {
                subcombinations = combinations(sum - i, size - 1, exclusions i)
                n = split(subcombinations, scs, "\n")
                for (j = 1; j <= n; j++) {
                    results[sortString(scs[j] " " i)] = 1
                }
            }
        }

        if (length(results) > 0) {
            n = asorti(results)
            return join(results, 1, n, "\n")
        }
    }
}

function isExcluded(exclusions, digit) {
    return index(exclusions, digit) != 0
}

# Sort a string of space-separated chars.
#
# ex: sortString("4 3 2 1") => "1 2 3 4"
#
function sortString(digitString,    n, chars) {
    n = split(digitString, chars)
    asort(chars)
    return join(chars, 1, n)
}

# Concatenate a range of fields in this record.
#
function concatenate(start, stop,    i, res) {
    res = ""
    for (i = start; i <= stop; i++)
        res = res $i
    return res
}
