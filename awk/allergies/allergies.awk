#!/usr/bin/env gawk -f

@include "join"
BEGIN {
    n = split("eggs peanuts shellfish strawberries tomatoes chocolate pollen cats", allergens, " ")
    for (i = 1; i <= n; i++) allergen[allergens[i]] = i - 1
    FS = OFS = ","
}

$2 == "allergic_to" { print is_allergic_to($3, $1) ? "true" : "false"}

$2 == "list" {
    delete allergies
    a = 0
    for (i = 1; i <= n; i++)
        if (is_allergic_to(allergens[i], $1))
            allergies[++a] = allergens[i]
    print join(allergies, 1, a, OFS)
}

function is_allergic_to(item, value) {
    return and(1, rshift(value, allergen[item])) == 1
}