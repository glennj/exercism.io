----------------------------------------
-- Robot simulator:
-- place a robot on a map and move it around
----------------------------------------

-- valid headings
local bearings = { east=0, north=90, west=180, south=270 }
local headings = {}
for k,v in pairs(bearings) do headings[v]=k end

----------------------------------------
-- the Robot class
local Robot = {}
Robot.__index = Robot

-- enable object creation with: r = Robot(...)
setmetatable(Robot, {
    __call = function(cls, ...) return cls:new(...) end,
})

function Robot:new(attributes)
    attributes = attributes or {}

    local robot = {
        x = attributes.x or 0,
        y = attributes.y or 0,
        heading = attributes.heading,
    }
    if not bearings[robot.heading] then robot.heading = 'north' end

    setmetatable(robot, self)
    return robot
end

function Robot:move(instructions)
    for instruction in instructions:gmatch('.') do
        if     instruction == 'A' then self:advance()
        elseif instruction == 'L' then self:turn(1)
        elseif instruction == 'R' then self:turn(-1)
        else   error('Unknown instruction')
        end
    end
end

function Robot:advance()
    local rad = math.rad(bearings[self.heading])
    self.x = self.x + math.cos(rad)
    self.y = self.y + math.sin(rad)
end

function Robot:turn(dir)
    local bearing = (bearings[self.heading] + dir*90) % 360
    self.heading = headings[bearing]
end

return Robot
