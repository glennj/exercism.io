local series_iterator = function(string, len)
    assert(len >= 0, "slice length cannot be negative")
    assert(len ~= 0, "slice length cannot be zero")
    assert(#string > 0, "series cannot be empty")
    assert(#string >= len, "slice length cannot be greater than series length")

    local idx = 0
    local limit = #string - len

    return function()
        if idx <= limit then
            idx = idx + 1
            return string:sub(idx, idx+len-1)
        end
    end
end

return series_iterator 

-- useful tutorial pages:
-- https://www.tutorialspoint.com/lua/lua_iterators.htm
-- http://lua-users.org/wiki/IteratorsTutorial 
