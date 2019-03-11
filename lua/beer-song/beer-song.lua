local MAX = 99
local LIQUID = "beer"
local WHERE = "on the wall"

local function bottle(n)
    return string.format("%s bottle%s of %s",
        n == 0 and "no more" or n,
        n == 1 and "" or "s",
        LIQUID
    )
end

local function capitalize(s)
    return s:sub(1,1):upper() .. s:sub(2)
end

local function task(n)
    return n == 0
        and "Go to the store and buy some more"
        or "Take " .. (n == 1 and "it" or "one") .. " down and pass it around"
end

local function verse(n)
    local b = bottle(n)
    local first = string.format("%s %s, %s.\n", capitalize(b), WHERE, b)

    b = bottle(n == 0 and MAX or n-1)
    local second = string.format("%s, %s %s.\n", task(n), b, WHERE)

    return first .. second
end

local function sing(from, to)
    from = from or MAX
    to = to or 0
    local verses = {}
    for i = from, to, -1 do table.insert(verses, verse(i)) end
    return table.concat(verses, "\n")
end

return { verse = verse, sing = sing }
