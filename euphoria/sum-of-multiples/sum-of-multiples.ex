include std/math.e      -- sum

enum false=0, true

public function sum_of_multiples(sequence factors, integer limit) 
    sequence multiples = {}
    for n = 1 to (limit - 1) do
        if has_factor(n, factors) then
            multiples &= n
        end if
    end for

    return sum(multiples)
end function

function has_factor(integer n, sequence f)
    for i = 1 to length(f) do
        if f[i] > 0 and remainder(n, f[i]) = 0 then
            return true
        end if
    end for
    return false
end function
