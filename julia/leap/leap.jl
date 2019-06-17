function is_leap_year(year::Int)
    divisibleby(n) = year % n == 0
    divisibleby(400) || (divisibleby(4) && !divisibleby(100))
end
