-- a few ways to do it...

local function reverse_string(s)
--[[
    local reversed = ""
    for i = #s, 1, -1 do
        reversed = reversed .. s:sub(i,i)
    end
    return reversed
--]]
    
--[[
    local reversed = {}
    for char in s:gmatch(".") do
        table.insert(reversed, 1, char)
    end
    return table.concat(reversed)
--]]

    local bytes = { string.byte(s, 1, #s) }
    local reversed = {}
    while next(bytes) do
        table.insert(reversed, table.remove(bytes))
    end
    return string.char(table.unpack(reversed))

end

return reverse_string
