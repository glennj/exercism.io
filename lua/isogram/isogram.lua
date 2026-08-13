local function is_isogram(input)
    local seen = {}
    -- remove all non-alpha
    local letters = (input or ""):upper():gsub("%A", "")

    for letter in letters:gmatch(".") do
        if seen[letter] then
            return false
        end
        seen[letter] = true
    end
    return true
end

return is_isogram
