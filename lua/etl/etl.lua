local function etl_transform(old)
    local new = {}
    for val, charlist in pairs(old) do
        for _, char in ipairs(charlist) do
            new[char:lower()] = val
        end
    end
    return new
end

return { transform = etl_transform }
