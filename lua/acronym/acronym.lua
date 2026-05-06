return function (text)
    return text
        -- 1. keep only the first letter of each word
        --    (apostrophe is a "word character")
        :gsub("%f[%a](.)[%a']*", "%1")

        -- 2. remove all non-letters
        :gsub("%A", "")

        -- 3. and uppercase
        :upper()
end
