-- Goal: use as few native Lua table operations as possible.
-- These first 3 functions use then, but none of the rest.

-- not required by the test suite
local push = function(list, elem)
    list[#list + 1] = elem
    return list
end

local function foldl(list, seed, f)
    local accumulator = seed
    for _, element in ipairs(list) do
        accumulator = f(accumulator, element)
    end
    return accumulator
end

local function append(list, elements)
    for _, element in ipairs(elements) do
        push(list, element)
    end
    return list
end

-- -----------------------------------------------------
-- the rest can be implemented with foldl and append
local function concat(...)
    return foldl({...}, {}, function(accum, list)
        return append(accum, list)
    end)
end

local function length(list)
    return foldl(list, 0, function(count, _)
        return count + 1
    end)
end

local function map(list, f)
    return foldl(list, {}, function(acc, elem)
        push(acc, f(elem))
        return acc
    end)
end

local function filter(list, f)
    return foldl(list, {}, function(acc, elem)
        if f(elem) then push(acc, elem) end
        return acc
    end)
end

local function reverse(list)
    return foldl(list, {}, function(reversed, elem)
        return append({elem}, reversed)
    end)
end

local function foldr(list, seed, f)
    return foldl(reverse(list), seed, function(acc, elem)
        return f(acc, elem)
    end)
end

return {
    append = append,
    concat = concat,
    filter = filter,
    foldl = foldl,
    foldr = foldr,
    length = length,
    map = map,
    reverse = reverse,
}
