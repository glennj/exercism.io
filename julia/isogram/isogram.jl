function isisogram(s::AbstractString)
    alpha = filter(isletter, lowercase(s))
    length(alpha) == length(Set(alpha))
end

"""
other nice community solutions

https://exercism.io/tracks/julia/exercises/isogram/solutions/642f56f99a704326b9afe0fcd98e421d

    function isisogram(s::AbstractString)
        allunique([c for c in lowercase(s) if isletter(c)])
    end

https://exercism.io/tracks/julia/exercises/isogram/solutions/96fb2369632447608a79aa4262b1010b

    function isisogram(s::AbstractString)
        s = filter(isalpha, lowercase(s))
        return collect(s) == unique(s)
    end

"""
nothing
