function distance(s1::AbstractString, s2::AbstractString)
    if length(s1) != length(s2)
        throw(ArgumentError("strings must be same length"))
    end

    """ First take, manually iterating through the strings:
    dist = 0
    i1 = nextind(s1, 0)
    i2 = nextind(s2, 0)
    while i1 ≤ length(s1) && i2 ≤ length(s2)
        if s1[i1] != s2[i2]
            dist += 1
        end
        i1 = nextind(s1, i1)
        i2 = nextind(s2, i2)
    end
    """

    # take 2, more functional
    count(map(≠, s1, s2))
end
