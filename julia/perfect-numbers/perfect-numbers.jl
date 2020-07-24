function aliquot_sum(num)
    if num < 1
        throw(DomainError(num, "Not a natural number"))
    end

    """
    sum = 0
    for f1 ∈ 1:isqrt(num)
        f2, r = divrem(num, f1)
        if r == 0
            sum += f1 + (f1 == f2 ? 0 : f2)
        end
    end
    # the sum of the factors not including the number itself
    sum - num
    """

    # more functionally
    sum(
        map(
            a -> a[1] + (a[1] == a[2] ? 0 : a[2]),
            filter(
                a -> a[3] == 0,
                [(f, div(num, f), mod(num, f)) for f in 1:isqrt(num)]
            )
        )
    ) - num
end


isperfect(num)   = num == aliquot_sum(num)
isabundant(num)  = num <  aliquot_sum(num)
isdeficient(num) = num >  aliquot_sum(num)
