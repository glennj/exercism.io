----------------------------------------
-- Robot simulator:
-- place a robot on a map and move it around
----------------------------------------

-- valid headings
local directions = { east=0, north=90, west=180, south=270 }
local headings = {}
for k,v in pairs(directions) do headings[v]=k end

-- dispatch table for instructing the robot to move
local motion = {}

-- A: advance 1 unit
function motion.A (robot)
    -- local rad = 2 * math.pi * directions[robot.heading] / 360
    local rad = math.rad(directions[robot.heading])
    robot.x = robot.x + math.cos(rad)
    robot.y = robot.y + math.sin(rad)
end

-- L: turn left (counter-clockwise)
function motion.L (robot, dir)
    dir = dir or 1
    robot.heading = headings[(directions[robot.heading] + dir*90) % 360]
end

-- R: turn right (clockwise)
function motion.R (robot)
    motion.L(robot, -1)
end

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
    if not directions[robot.heading] then robot.heading = 'north' end

    setmetatable(robot, self)
    return robot
end

function Robot:move(instructions)
    for instruction in instructions:gmatch('.') do
        assert(motion[instruction], 'Unknown instruction')
        motion[instruction](self)
    end
end

return Robot
