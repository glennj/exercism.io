
function rotate(n::Int, source::Char)::Char
    mapping = create_rotation(n)
    get(mapping, source, source)
end

function rotate(n::Int, source::String)::String
    mapping = create_rotation(n)
    map(c -> get(mapping, c, c), source)
end


function create_rotation(n::Int)::Dict{Char, Char}
    n %= 26

    alphabet = collect('a':'z') 
    rotated = collect(alphabet[n+1:end])
    append!(rotated, alphabet[1:n])

    pairs = []
    for i in 1:length(alphabet)
        push!(pairs, (alphabet[i], rotated[i]))
        push!(pairs, (uppercase(alphabet[i]), uppercase(rotated[i])))
    end
    Dict(pairs)
end

#macro R13_str(s) rotate(13, s) end

# thanks to bwasty for the use of `Symbol`
# https://exercism.io/tracks/julia/exercises/rotational-cipher/solutions/2c423955e21c45d28ad20d04af99cbbb

for i in 1:26
    name = Symbol("R", i, "_str")
    eval(quote
        macro $name(s) rotate($i, s) end
    end)
end
