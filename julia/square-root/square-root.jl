# Using the Binary numeral system (base 2) from Wikipedia
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
#
function square_root(n)
    # find b, the greatest power of 4 <= n
    b = 4 ^ floor(Int, log(4, n))
    x = 0

    while b != 0
        if n >= x + b
            n = n - x - b
            x = x ÷ 2 + b
        else
            x = x ÷ 2
        end
        b = b ÷ 4
    end

    x
end
