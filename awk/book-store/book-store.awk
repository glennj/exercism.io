#! /usr/bin/env gawk -f

@include "arrays"

BEGIN {
    BookPrice = 800
    split("1.0 0.95 0.90 0.80 0.75", Discounted)

    # My arrays module does not yet handle multidimensional arrays
    bundles[1][1] = 1
    delete bundles[1][1]
}

# Add the book number to a bundle.
# A bundle contains no duplicate books.
{
    for (i in bundles) {
        if (!arrays::contains(bundles[i], $1)) {
            arrays::push(bundles[i], $1)
            next
        }
    }
    # Book exists in all current bundles: create a new one
    bundles[length(bundles) + 1][1] = $1
}

# optimize the bundles: two 4-bundles is cheaper
# than a 5-bundle plus a 3-bundle
function optimize(          i, b5, b3) {
    for (i in bundles) {
        if (length(bundles[i]) == 5) b5 = i
        if (length(bundles[i]) == 3) b3 = i
    }
    if (!b5 || !b3) return

    for (i in bundles[b5]) {
        if (!arrays::contains(bundles[b3], bundles[b5][i])) {
            arrays::push(bundles[b3], bundles[b5][i])
            delete bundles[b5][i]
            break
        }
    }
    # keep going until no more (5-bundle + 3-bundle)
    optimize()
}

END {
    optimize()

    price = 0
    for (i in bundles) {
        size = length(bundles[i])
        price += size * BookPrice * Discounted[size]
    }
    print price
}
