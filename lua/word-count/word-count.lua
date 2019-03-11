local function word_count(sentence)
    sentence = sentence or ""
    local count = {}
    for word in sentence:lower():gmatch("%w+") do
        count[word] = (count[word] or 0) + 1
    end
    return count
end

return { word_count = word_count }
