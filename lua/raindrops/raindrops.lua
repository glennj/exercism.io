local drops = {{3, "Pling"}, {5, "Plang"}, {7, "Plong"}}

local raindrops = function(n)
    local result = ""
    for _, pair in ipairs(drops) do
        if n % pair[1] == 0 then
            result = result .. pair[2]
        end
    end
    return #result > 0 and result or tostring(n)
end

return raindrops
