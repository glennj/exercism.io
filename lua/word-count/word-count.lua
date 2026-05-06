local function word_count(sentence)
    local count = {}
    for word in (sentence or ""):lower():gmatch("[%w']+") do
        local w = word:trim("'")
        if #w > 0 then
            count[w] = (count[w] or 0) + 1
        end
    end
    return count
end


-- monkeypatch some methods into the string package
string.rtrim = function (s, charset)
    return s:gsub((charset or '%s').."+$", "")
end
string.ltrim = function (s, charset)
    return s:gsub("^"..(charset or '%s').."+", "")
end
string.trim = function (s, charset)
    return s:ltrim(charset):rtrim(charset)
end


return { count_words = word_count }
