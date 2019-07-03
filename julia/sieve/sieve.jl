function sieve(limit::Integer)
    nonprime = zero(limit)
    sieve = collect(1:limit)
    sieve[1] = nonprime
    i = 2
    while i^2 <= limit
        step = i * (i == 2 ? 1 : 2)
        for j in i^2:step:limit
            sieve[j] = nonprime
        end
        i += 1
    end
    filter(!iszero, sieve)
end
