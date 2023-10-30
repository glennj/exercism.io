local rle = {}

--------------------------------------------------------------------
rle.encode = function(input)
    -- get the unique characters in input
    local uniq = {}
    for char in input:gmatch(".") do
        uniq[char] = 1
    end

    -- encode runs of 2 or more of those characters
    local encoded = input
    for char,_ in pairs(uniq) do
          local pattern = char .. char .. "+"
          encoded = encoded:gsub(pattern, function(s)
              return tostring(#s) .. s:sub(1,1)
          end)
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
