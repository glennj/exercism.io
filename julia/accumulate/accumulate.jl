function accumulate(xs, op)
    #= first pass, a loop
    result = []
    for x in xs
        push!(result, op(x, x))
    end
    result
    =#

    #= take 2, list comprehension
    [op(x, x) for x in xs]
    =#

    # third, vectorization
    op.(xs, xs)

end
