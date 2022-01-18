function collatz_steps(n::Integer)
    n > 0 || throw(DomainError("."))
    steps = 0
    while n > 1
        n = iseven(n) ? div(n, 2) : 3n + 1
        steps += 1
    end
    steps
end
