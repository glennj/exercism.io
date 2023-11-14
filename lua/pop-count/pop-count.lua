local PopCount = {}

function PopCount.eggCount(number)
    local count = 0
    while number > 0 do
        -- bitwise operators only showed up in Lua 5.3
        if number & 1 == 1 then
            count = count + 1
        end
        number = number >> 1
    end
    return count
end

return PopCount
