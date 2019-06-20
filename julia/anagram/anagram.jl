function detect_anagrams(subject::AbstractString, candidates::AbstractArray)
    tokey = s -> split(s, "") |> sort |> join

    ls = lowercase(subject)
    key = tokey(ls)

    """ take 1
    anagrams = []
    for candidate in candidates
        lc = lowercase(candidate)
        if ls != lc && key == tokey(lc)
            push!(anagrams, candidate)
        end
    end
    anagrams
    """

    # take 2
    filter(candidates) do candidate
        lc = lowercase(candidate)
        ls != lc && key == tokey(lc)
    end
end
