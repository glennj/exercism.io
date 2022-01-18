function isisogram(s::AbstractString)
    alpha = filter(isletter, lowercase(s))
    length(alpha) == length(Set(alpha))
end
