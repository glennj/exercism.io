function square_of_sum(n::Int)
    sum(1:n) ^ 2
end


function sum_of_squares(n::Int)
    #sum(map(i -> i^2, 1:n))
    sum((1:n) .^ 2)
end


function difference(n::Int)
    square_of_sum(n) - sum_of_squares(n)
end
