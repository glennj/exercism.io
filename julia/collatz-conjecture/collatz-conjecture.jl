#function collatz_steps(n::Integer)
#    n > 0 || throw(DomainError(n))
#    steps = 0
#    while n > 1
#        n = iseven(n) ? div(n, 2) : 3n + 1
#        steps += 1
#    end
#    steps
#end



import Base: iterate

struct Collatz
    start::Integer

    function Collatz(n::Integer)
        n > 0 || throw(DomainError(n))
        new(n)
    end
end

Base.IteratorSize(Collatz) = Base.SizeUnknown()

function next_collatz(n::Integer)
    if n == 1
        nothing
    else
        iseven(n) ? div(n, 2) : 3n + 1
    end
end 

function iterate(c::Collatz) 
    (c.start, next_collatz(c.start))
end

function iterate(c::Collatz, state::Union{Nothing, Integer})
    if isnothing(state)
        nothing
    else
        (state, next_collatz(state))
    end
end

function collatz_steps(n::Integer)
    length(collect(Collatz(n))) - 1
end
