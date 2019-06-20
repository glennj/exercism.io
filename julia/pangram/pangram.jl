
function ispangram(input::AbstractString)
    letters = filter(isletter, lowercase(input)) |> unique
    all(c -> c in letters, 'a':'z')
end
