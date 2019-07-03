function transpose_strings(input::AbstractArray)
    isempty(input) && return []

    maxlen = maximum(map(length, input))

    # array of arrays of characters
    chars = [ [c for c in rpad(s, maxlen)] for s in input]

    # transpose the chars array
    transposed = [
        [ chars[row][col] for row in 1:length(input) ]
        for col in 1:maxlen
    ]

    # join the arrays of chars into strings
    output = map(join, transposed)

    # ensure each strings is at least as long as 
    # the _next_ one: loop from the bottom up
    output[maxlen] = rstrip(output[maxlen])
    for i in maxlen-1:-1:1
        output[i] = rpad(rstrip(output[i]), length(output[i+1]))
    end

    output
end
