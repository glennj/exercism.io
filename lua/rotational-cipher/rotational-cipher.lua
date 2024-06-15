local alphabet = "abcdefghijklmnopqrstuvwxyz"

local function cryptMap(key)
    local crypt = alphabet:sub(key+1) .. alphabet:sub(1, key)
    local map = {}
    for i = 1, #alphabet do
        map[alphabet:sub(i,i)] = crypt:sub(i,i)
        map[alphabet:sub(i,i):upper()] = crypt:sub(i,i):upper()
    end
    return map
end

return {
    rotate = function(input, key)
        local mapping = cryptMap(key)
        local encoded = {}
        for char in input:gmatch(".") do
            encoded[#encoded + 1] = mapping[char] or char
        end
        return table.concat(encoded)
    end
}
