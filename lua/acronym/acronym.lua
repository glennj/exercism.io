return function (text)
    return text
        -- 1. captilize all words
        :gsub("%f[%a]%l", string.upper)

        -- 2. add space between a lower and an upper
        :gsub("%f[%L]%f[%u]", " ")

        -- 3. take only the first letter of each word
        :gsub("%u%S+", function(word) return word:sub(1,1) end)

        -- 4. and remove all spaces
        :gsub("%s", "")
end
