local function word_count(sentence)
    sentence = sentence or ""
    local count = {}
    for word in sentence:lower():gmatch("[%w']+") do
        -- remove leading/trailing quote
        word = word:gsub("^'", ""):gsub("'$", "")
        count[word] = (count[word] or 0) + 1
    end
    return count
end

rtrim = function (s, charset)
    return s:gsub((charset or '%s').."*$", "")
end
ltrim = function (s, charset)
    return s:gsub((charset or '%s').."*$", "")
end
trim = function (s, charset)
    return rtrim(ltrim(s, charset))
end

return { word_count = word_count }
