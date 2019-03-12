local VOWELS = "AaEeIiOoUu"
local VOWEL     = "[" ..VOWELS.."]"
local CONSONANT = "[^"..VOWELS.."]"

local PATTERNS = {
    vowel = {
        "^"..VOWEL,
        "^[Xx][Rr]",
        "^[Yy][Tt]",
    },
    consonant = {
        "^(.?[Qq][Uu])(.*)",           -- "square" => "aresquay", "quip" => "ipquay"
        "^("..CONSONANT.."+)([Yy].*)", -- "rhythm" => "ythmrhay", "my" => "ymay"
        "^("..CONSONANT.."+)(.*)",     -- "strengths" => "engthsstray"
    },
}

local pig_latinize

local to_pig_latin = function(phrase)
    return phrase:gsub("%w+", pig_latinize)
end

pig_latinize = function(word)
    for _, patt in ipairs(PATTERNS.vowel) do
        if word:find(patt) then
            return word.."ay"
        end
    end

    for _, patt in ipairs(PATTERNS.consonant) do
        local first, rest = word:match(patt)
        if first and rest then
            return rest..first.."ay"
        end
    end

    -- are there any other cases?
    return word.."ay"
end

return to_pig_latin

