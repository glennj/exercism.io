local Set = require("Set")  -- https://wscherphof.github.io/lua-set/ 
                            -- luarocks install set

local alphabet = 'abcdefghijklmnopqrstuvwxyz'

local function is_pangram(input)
    input = input or ""
    local set = Set:new()
    for letter in input:upper():gmatch("%a") do
        set:add(letter)
    end
    return set:len() == #alphabet
end

return is_pangram
