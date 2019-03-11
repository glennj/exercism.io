local Set = require("Set")  -- https://wscherphof.github.io/lua-set/ 

local function is_isogram(input)
    -- remove all non-alpha
    local letters = (input or ""):upper():gsub("%A", "")

    local set = Set:new()
    for letter in letters:gmatch(".") do
        set:add(letter)
    end
    return set:len() == #letters
end

return is_isogram
