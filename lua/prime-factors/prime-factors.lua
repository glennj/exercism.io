-- Prime Factorization
-- prime_factors(300) is {2, 2, 3, 5, 5}

return function (n)
    local factors = {}
    local f = 2

    while f * f <= n do
        if n % f == 0 then
            factors[#factors+1] = f
            n = n / f
        else
            f = f + 1
            -- or, we can use a prime number generator
            -- to get the next factor
        end
    end

    if n > 1 then
        factors[#factors+1] = n
    end

    return factors
end
