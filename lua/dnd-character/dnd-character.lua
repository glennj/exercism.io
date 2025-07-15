local function ability()
    local d6 = function() return math.random(6) end
    local sum = 0
    local min = 6
    for _, die in ipairs({ d6(), d6(), d6(), d6() }) do
        sum = sum + die
        min = math.min(min, die)
    end
    return sum - min
end

local function modifier(input)
    return math.floor((input - 10) / 2)
end

-- ------------------------------------------------------------
local characteristics = {
    "strength", "dexterity", "constitution", "intelligence", "wisdom", "charisma"
}

-- Full OO is overkill, but c'est la vie
local Character = {}
Character.__index = Character

function Character:new(name)
    local c = {}
    setmetatable(c, self)
    c.name = name
    for _, characteristic in ipairs(characteristics) do
        c[characteristic] = ability()
    end
    c.hitpoints = 10 + modifier(c.constitution)
    return c
end

-- ------------------------------------------------------------
return {
    Character = Character,
    ability = ability,
    modifier = modifier
}
