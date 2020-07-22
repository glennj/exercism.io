function prime_factors(n::Int)
    factors = []
    if n < 2
        return factors
    end

    f = 2
    while f ≤ √n
        if rem(n, f) == 0
            append!(factors, f)
            n ÷= f
        else
            f += 1
        end
    end

    if n > 0
        append!(factors, n)
    end

    factors
end
