local divBy = function (num, den) return num % den == 0 end

-- leap year
return function(year)
    return divBy(year, 4) and (not divBy(year, 100) or divBy(year, 400))
end
