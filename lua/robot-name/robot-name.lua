local Set = require("Set")  -- https://luarocks.org/modules/luarocks/set
                            -- luarocks install set

local NamesDB = Set:new()

local random_name = function() 
    local letter = function()
        return string.char(("A"):byte() + math.random(0,25))
    end
    local digits = function()
        return ("%03d"):format(math.random(0,999))
    end
    return letter()..letter()..digits()
end

local designate = function(robot)
    repeat
        robot.name = random_name()
    until not NamesDB[robot.name]
    NamesDB:add(robot.name)
    return robot
end

--------------------------------------------
local Robot = {}
Robot.__index = Robot

function Robot:new()
    return setmetatable(designate({}), self)
end

function Robot:reset()
    designate(self)
end


return Robot
