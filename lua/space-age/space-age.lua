local seconds_per_earth_year = 31557600
local relative_orbital_periods = {
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

function SpaceAge:new(age_in_seconds)
    local age = {}
    setmetatable(age, {
        -- functions are assigned directly to the instance (below), 
        -- so any reason to look up a "missing" function is an error.
        __index = function() error('not a planet') end
    })

    for planet, earth_years_per_planet_year in pairs(relative_orbital_periods) do
        local funcname = "on_" .. planet
        local age_on_planet = float2fixed(
            age_in_seconds / seconds_per_earth_year / earth_years_per_planet_year
        )
        age[funcname] = function(self) return age_on_planet end
    end

    return age
end

return SpaceAge
