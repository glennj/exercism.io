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
    if input == "" then 
        return 'Fine, be that way.'
    end

    local shouting = is_shouting(input)
    local asking   = is_question(input)

    if shouting and asking then
        return "Calm down, I know what I'm doing!"
    elseif shouting then
        return 'Whoa, chill out!'
    elseif asking then
        return 'Sure'
    else 
        return 'Whatever'
    end
end

return bob
