# These variables are initialized on the command line (using '-v'):
# - sum

BEGIN {
    OFS = ","

    a = 0
    while (1) {
        a++

        # derived from
        #   a + b + c = sum
        #   a^2 + b^2 = c^2
        b = (sum * (sum - 2 * a)) / (2 * (sum - a))

        if (b < a)
            break
        else if (b == int(b))
            print a, b, sum - a - b
    }
}
