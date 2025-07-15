-- will this work with wide chars?
local function sorted_chars(str)
    local chars = {}
    for c in string.gmatch(str, ".") do 
        table.insert(chars, c)
    end
    table.sort(chars)
    return table.concat(chars)
end

-- Anagram class
local Anagram = {}
Anagram.__index = Anagram

function Anagram:new(base)
    local anagram = {}
    setmetatable(anagram, self)
    self.base = base:lower()
    self.key = sorted_chars(self.base)
    return anagram
end

function Anagram:match(words)
    local matches = {}
    for _, word in pairs(words) do
        local lc = word:lower()
        if self.base ~= lc and self.key == sorted_chars(lc) then
            table.insert(matches, word)
        end
    end
    return matches
end

return Anagram
