function sum_of_multiples(limit, factors)
	fs          = filter(!iszero, factors)
	is_multiple = i -> any(f -> i % f == 0, fs)
	multiples   = a -> filter(is_multiple, a)

	1:limit-1 |> multiples |> sum
end
