local seconds_per_earth_year = 31557600
local relative_orbital_period = {
    mercury =   0.2408467,
    venus   =   0.61519726,
    earth   =   1.0,
    mars    =   1.8808158,
    jupiter =  11.862615,
    saturn  =  29.447498,
    uranus  =  84.016846,
    neptune = 164.79132,
}

-- restrict a number to 2 decimal places, clunkily
function float2fixed(f)
    return tonumber(string.format("%.2f", f))
end

local SpaceAge = {}
SpaceAge.__index = SpaceAge

function SpaceAge:new(age_in_seconds)
    local age = {
        seconds = age_in_seconds or 0,
    }
    setmetatable(age, self)

    for planet, period in pairs(relative_orbital_period) do
        local funcname = "on_" .. planet
        local relative_age = float2fixed(
            age_in_seconds / seconds_per_earth_year / period
        )
        SpaceAge[funcname] = function(self) return relative_age end
    end

    return age
end

return SpaceAge
