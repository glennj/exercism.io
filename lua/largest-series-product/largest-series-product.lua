local function digits(str)
    local ds = {}
    for char in str:gmatch(".") do
        local d = tonumber(char)
        assert(d, "invalid digit")
        table.insert(ds, d)
    end
    return ds
end

local function slice(list, i, j)
    return { table.unpack(list, i, j) }
end

local function each_span(list, len)
    local iterator = function()
        for i = 1, #list - len + 1 do
            coroutine.yield(slice(list, i, i+len-1))
        end
    end
    return coroutine.wrap(iterator)
end

local function product(list)
    local product = 1
    for _, digit in ipairs(list) do
        product = product * digit
    end
    return product
end

local function largest_series_product(params)
    assert(params and params.digits and params.span, "invalid arguments")
    assert(#params.digits >= params.span, "span too long")
    assert(params.span >= 0, "span too short")

    local largest = math.mininteger
    for span in each_span(digits(params.digits), params.span) do
        largest = math.max(largest, product(span))
    end
    return largest
end

return largest_series_product
