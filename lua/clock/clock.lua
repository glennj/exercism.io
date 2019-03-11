local Clock = {}
Clock.__index = Clock

function Clock:new(hours, minutes)
    local clock = {}
    setmetatable(clock, Clock)
    clock.h, clock.m = self:normalize(hours, minutes)
    return clock
end

-- noramalize the time: minute in [0,60); hour in [0,24)
function Clock:normalize(hours, minutes)
    return (hours + math.floor(minutes / 60)) % 24, minutes % 60
end

function Clock:__tostring()
    return string.format("%02d:%02d", self.h, self.m)
end

function Clock:plus(minutes)
    minutes = minutes or 0
    self.h, self.m = self:normalize(self.h, self.m + minutes)
    return self
end

function Clock:minus(minutes)
    return self:plus(-1 * (minutes or 0))
end

function Clock:equals(other)
    return tostring(self) == tostring(other)
end

--

local at = function(hours, minutes)
    return Clock:new(hours or 0, minutes or 0)
end

return { at = at }
