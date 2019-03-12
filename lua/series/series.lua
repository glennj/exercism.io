local series_iterator = function(string, len)
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
