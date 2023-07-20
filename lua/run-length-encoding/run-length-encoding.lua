local rle = {}

--------------------------------------------------------------------
-- Other languages, those with more complete regex implmementations,
-- allow backreferences in the pattern. For example, Julia can do
--[[
        function encode(s)
            function shrink(s)
                len = length(s)
                num = len == 1 ? "" : string(len)
                num * s[1]
            end

            replace(s, r"(.)\1*" => shrink)
        end
--]]
-- However, Lua's patterns are simpler, so we iterate over the patterns.
--------------------------------------------------------------------

rle.encode = function(input)
    local encoded = input
    -- an optimization: if we see 100 A's we only need to
    -- globally replace when we see the first one.
    local seen = {}

    for char in input:gmatch(".") do
        if not seen[char] then
            -- ensure the pattern captures runs of 2 or more
            local pattern = char .. char .. "+"
            encoded = encoded:gsub(pattern, function(s)
                return tostring(#s) .. s:sub(1,1)
            end)
            seen[char] = true
        end
    end
    return encoded
end

--------------------------------------------------------------------
rle.decode = function(input)
    return input:gsub("(%d+)(%D)", function(n, c)
        return c:rep(tonumber(n))
    end)
end

--------------------------------------------------------------------
return rle
