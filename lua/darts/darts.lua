local Darts = {}

local function hypot(x, y)
    return math.sqrt(x^2 + y^2)
end


function Darts.score(x, y)
    local dist = hypot(x, y)
    if dist <=  1 then return 10 end
    if dist <=  5 then return  5 end
    if dist <= 10 then return  1 end
    return 0
end

return Darts
