local function is_question(input)
    -- ends with a question mark
    return input:find("%?$")
end

local function is_shouting(input)
    -- has alpha chars and no lowercase alphas
    return input:find("%a") and not input:find("%l")
end

--
local bob = {}

function bob.hey(input)
    local trimmed = input:gsub("%s+$", "")

    local silence  = #trimmed == 0
    local shouting = is_shouting(trimmed)
    local asking   = is_question(trimmed)

    if     silence             then return 'Fine. Be that way!'
    elseif shouting and asking then return "Calm down, I know what I'm doing!"
    elseif shouting            then return 'Whoa, chill out!'
    elseif asking              then return 'Sure.'
    else                            return 'Whatever.'
    end
end

return bob
