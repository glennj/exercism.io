"""
Consider the 3x3 array: Julia indexes this like
1 4 7
2 5 8
3 6 9

We want to populate this like
1 8 7
2 9 6
3 4 5

With the idx initialized to zero, for a length of 3, we 
increment idx by 1; then for a length of 2, we increment
by 3; then for a length of 2 we increment by -1; then
for a length of 1 we increment by -3; then for a length of 1
we increment by 1.

For the length delta, we have a pattern of 
(substract 1, subtract 0) and repeat.

For the index delta, we have a pattern of
(add 1, add N, subtract 1, subtract N) and repeat.
"""
function spiral_matrix(n::Int)
    a = Array{typeof(n), 2}(undef, n, n)

    idx_delta = Iterators.cycle([1, n, -1, -n])
    (Δidx, Δidx_state) = iterate(idx_delta) 

    len_delta = Iterators.cycle([1, 0])
    (Δlen, Δlen_state) = iterate(len_delta)

    len = n
    idx = 0
    i = 1

    while i ≤ n^2
        for _ in 1:len
            idx += Δidx
            a[idx] = i
            i += 1
        end
        len -= Δlen

        (Δidx, Δidx_state) = iterate(idx_delta, Δidx_state)
        (Δlen, Δlen_state) = iterate(len_delta, Δlen_state)
    end

    transpose(a)
end
