#= iterative
function eggcount(number)
    count = 0
    while number > 0
        count += number & 1
        number >>= 1
    end
    count
end
=#

# recursive
function eggcount(number, count=0)
    if number <= 0
        return count
    end
    return eggcount(number >> 1, count + (number & 1))
end
