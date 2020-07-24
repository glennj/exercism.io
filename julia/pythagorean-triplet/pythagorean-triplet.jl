# Algorithm stolen from Python example.py

module PythoagoreanTriplets
    function triplets_with_sum(perimeter)
        triplets = []
        for triplet ∈ triplets_in_range(1, perimeter ÷ 2)
            if sum(triplet) == perimeter
                push!(triplets, triplet)
            end
        end
        sort(triplets)
    end

    function triplets_in_range(from, to)
        triplets = []
        for limit ∈ 4:4:to
            for (a, b, c) ∈ primitive_triplets(limit)
                da, db, dc = a, b, c
                while a < from
                    a += da
                    b += db
                    c += dc
                end
                while c ≤ to
                    push!(triplets, (a, b, c))
                    a += da
                    b += db
                    c += dc
                end
            end
        end
        triplets
    end

    function primitive_triplets(limit)
        triplets = []
        for (m, n) ∈ euclidian_coprimes(limit)
            a = m^2 - n^2
            b = 2 * m * n
            c = m^2 + n^2
            if a > b
                b, a = a, b
            end
            push!(triplets, (a, b, c))
        end
        triplets
    end
    
    function euclidian_coprimes(limit)
        pairs = []
        mn = limit ÷ 2
        for n ∈ 1:isqrt(mn)
            if mod(mn, n) == 0
                m = mn ÷ n
                if mod((m - n), 2) == 1 && gcd(m, n) == 1
                    push!(pairs, (m, n))
                end
            end
        end
        pairs
    end

end

function pythagorean_triplets(perimeter)
    PythoagoreanTriplets.triplets_with_sum(perimeter)
end

# this returns in under 2 seconds


################################################################################
# brute force solution
function pythagorean_triplets_brute(perimeter)
    # as a list comprehension
    [(a, b, c)
        for c ∈ (perimeter-3-4):-1:5
        for b ∈ (c-1):-1:4
        for a ∈ [perimeter - b - c]
        if a ≥ 3 && a < b && hypot(a, b) == c
    ]

    #= with loops
    result = []
    for c ∈ (perimeter-3-4):-1:5
        for b ∈ (c-1):-1:4
            a = perimeter - b - c
            if a ≥ 3 && a < b && hypot(a, b) == c
                push!(result, (a, b, c))
            end
        end
    end
    result
    =#
end

# brute force runs in under 4 seconds
