local validate
local span_products
local product_of_digits

local largest_series_product = function (params)
    validate(params)
    local prods = {}
    for prod in span_products(params.digits, params.span) do
        prods[#prods+1] = prod
    end
    return math.max(table.unpack(prods))
end

validate = function(params)
    assert(params and params.digits and params.span)
    assert(not params.digits:find("%D"))
    assert(#params.digits >= params.span)
    assert(params.span >= 0)
end

span_products = function(string, len)
    return coroutine.wrap(function()
        for i = 1, #string - len + 1 do
            local s = string:sub(i, i+len-1)
            coroutine.yield(product_of_digits(s))
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
