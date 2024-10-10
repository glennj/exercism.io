public function pythagorean_triplets(integer n) 
    sequence triplets = {}
    integer a = 2
    while 1 do
        a += 1
        -- formula below is derived from c^2 = a^2 + b^2
        -- and substituting c = n - a - b
        atom b = n * (n - 2 * a) / (2 * (n - a))
        if a >= b then
            exit
        elsif integer(b) then
            integer c = n - a - b
            triplets = append(triplets, {a, b, c})
        end if
    end while
    return triplets
end function
