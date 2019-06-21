module Atbash
    export encode, decode

    # map 'a' to 'z', 'b' to 'y', etc
    mapping = Dict(zip('a':'z', 'z':-1:'a'))

    for digit in '0':'9'
        mapping[digit] = digit      # digits map to themselves
    end

    function xcode(input::AbstractString)
        join(get(mapping, c, "") for c in lowercase(input))
    end

    function group(s::AbstractString, size::Int=5)
        re = Regex(".{1,$size}")
        join([m.match for m in eachmatch(re, s)], " ")
    end

    function encode(input::AbstractString)
        xcode(input) |> group
    end

    function decode(input::AbstractString)
        xcode(input)
    end
end

# pull the exported functions into the global scope
using .Atbash
