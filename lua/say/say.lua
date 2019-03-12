-- translation of this lovely recursive solution
-- https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

local SMALL = {
    [0]='zero', 'one', 'two', 'three', 'four', 'five', 'six',
    'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve',
    'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen',
    'eighteen', 'nineteen',
}

local XTY = {
    [20]='twenty', [30]='thirty',  [40]='forty',  [50]='fifty',
    [60]='sixty',  [70]='seventy', [80]='eighty', [90]='ninety',
}

local say_small
local say_compound

local say = function(n)
    if n < 0 or n >= 1e12 then return -1 end

    if     n < 100 then return say_small(n)
    elseif n < 1e3 then return say_compound(n, 100, 'hundred')
    elseif n < 1e6 then return say_compound(n, 1e3, 'thousand')
    elseif n < 1e9 then return say_compound(n, 1e6, 'million')
    else                return say_compound(n, 1e9, 'billion')
    end
end

say_small = function(n)
    return SMALL[n]
        or XTY[n]
        or XTY[n - n%10] .. '-' .. SMALL[n%10]
end

say_compound = function(n, base, word)
    local quot, rem = n // base, n % base
    local said = say(quot) .. ' ' .. word
    if rem > 0 then
        said = said .. ' ' .. say(rem)
    end
    return said
end

return say
