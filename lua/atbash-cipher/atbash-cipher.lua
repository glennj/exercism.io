local byte = {a = ("a"):byte(), z = ("z"):byte()}

local encode_char = function (char)
    return string.char(byte.z - char:byte() + byte.a)
end

local group_by = function (string, n)
    n = n or 5
    return string:gsub(("."):rep(n), "%0 "):gsub("%s$", "")
end

----------------------------------
local atbash = function(plaintext)
    local alnum_chars = (plaintext or ""):gsub("%W", ""):lower()
    local encoded = alnum_chars:gsub("%l", encode_char)
    return group_by(encoded)
end

return { encode = atbash }
