local CrypoSquare = {}

CrypoSquare.normalized_plaintext = function(string)
    return (string or ""):lower():gsub("%W", "")
end

CrypoSquare.size = function(string, is_normalized)
    local len = #string
    if not is_normalized then
        len = #CrypoSquare.normalized_plaintext(string)
    end
    return math.ceil(math.sqrt(len))
end

CrypoSquare.segments = function(string)
    local text = CrypoSquare.normalized_plaintext(string)
    local size = CrypoSquare.size(text, true)
    local segments = {}
    if #text > 0 then
        for i = 1, #text, size do
            segments[#segments+1] = text:sub(i, i+size-1)
        end
    end
    return segments
end

CrypoSquare.ciphertext = function(string)
    local segments = CrypoSquare.segments(string)
    if #segments == 0 then return "" end
    -- transpose the segments
    local columns = {}
    for c = 1, #segments[1] do
        local column = {}
        for r = 1, #segments do
            column[r] = segments[r]:sub(c,c)
        end
        columns[#columns+1] = table.concat(column)
    end
    return table.concat(columns)
end

CrypoSquare.normalized_ciphertext = function(string)
    local ciphertext = CrypoSquare.ciphertext(string)

    -- Determine the size of each normalized word:
    -- each word is default len(ciphertext) / size
    -- and the remainder is distributed over the first few.
    local size = CrypoSquare.size(ciphertext, true)
    local len = #ciphertext // size
    local lengths = {}
    for i = 1, size do
        lengths[#lengths+1] = len
    end
    for i = 1, #ciphertext % size do
        lengths[i] = lengths[i] + 1
    end

    local segments = {}
    local idx = 1
    for i = 1, #lengths do
        segments[#segments+1] = ciphertext:sub(idx, idx + lengths[i]-1)
        idx = idx + lengths[i]
    end

    return table.concat(segments, " ")
end

return CrypoSquare
