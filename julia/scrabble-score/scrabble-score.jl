function score(str::AbstractString)
    isempty(str) && return 0

    tiles = Dict(union(
        [(c,  1) for c in "AEIOULNRST"],
        [(c,  2) for c in "DG"],
        [(c,  3) for c in "BCMP"],
        [(c,  4) for c in "FHVWY"],
        [(c,  5) for c in "K"],
        [(c,  8) for c in "JX"],
        [(c, 10) for c in "QZ"],
    ))

    sum( get(tiles, c, 0) for c in uppercase(str) )
end
