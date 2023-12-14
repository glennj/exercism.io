local validate
local span_products
local product_of_digits

local largest_series_product = function (params)
    validate(params)
    local max = math.mininteger
    for span in spans(params.digits, params.span) do
        max = math.max(max, product_of_digits(span))
    end
    return max
end

validate = function(params)
    assert(params and params.digits and params.span)
    assert(not params.digits:find("%D"))
    assert(#params.digits >= params.span)
    assert(params.span >= 0)
end

spans = function(string, len)
    return coroutine.wrap(function()
        for i = 1, #string - len + 1 do
            coroutine.yield(string:sub(i, i+len-1))
        end
    end)
end

product_of_digits = function(string)
    local product = 1
    for char in string:gmatch("%d") do
        product = product * tonumber(char)
    end
    return product
end

return largest_series_product
