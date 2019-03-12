-- not required by the test suite, but I use it later
local push = function(list, elem)
    list[#list + 1] = elem
    return list
end

-- reduce a list to a single value
local reduce = function (list, seed, f)
    local accumulator = seed
    for _, element in ipairs(list) do
        accumulator = f(element, accumulator)
    end
    return accumulator
end

-- both map and filter can be implemented with reduce

local map = function (list, f)
    return reduce(list, {}, function(elem, acc)
        push(acc, f(elem))
        return acc
    end)
end

local filter = function (list, f)
    return reduce(list, {}, function(elem, acc)
        if f(elem) then push(acc, elem) end
        return acc
    end)
end

return {
    map = map,
    reduce = reduce,
    filter = filter,
}
