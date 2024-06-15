# awk implements bitwise functionality with functions, not operators.
# https://www.gnu.org/software/gawk/manual/html_node/Bitwise-Functions.html

function popCount(n,    c) {
    c = 0
    while (n > 0) {
        c += and(n, 1)
        n = rshift(n, 1)
    }
    return c
}

NF {print popCount($1)}
