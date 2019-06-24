function trinary_to_decimal(str::AbstractString)
    if occursin(r"[^012]", str)
        0
    else
        reduce(+, parse(Int, c) * 3^(i - 1) 
                  for (i, c) in enumerate(reverse(str))
        )
    end
end
