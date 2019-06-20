function triangle(n::Int)
    n < 0 && throw(DomainError("must be non-negative"))

    """ take 1: a 2-D array approach
    result = []
    prev = []

    for i in 1:n
        row = [1]
        for j in 2:i-1
            push!(row, prev[j-1] + prev[j])
        end
        if i > 1
            push!(row, 1)
        end
        push!(result, row)
        prev = row
    end
    """

    """ take 2: using binomial coefficients in loops
    result = []
    for i in 1:n
        row = []
        for j in 1:i
            push!(row, binomial(i-1, j-1))
        end
        push!(result, row)
    end
    return result
    """
    
    # take 3, nested comprehensions
    [ [ binomial(i-1, j-1) for j in 1:i ] for i in 1:n ]

end
