local byte = {a = ("a"):byte(), z = ("z"):byte()}

local encode_char = function (char)
    return string.char(byte.z - char:byte() + byte.a)
end

local group_by = function (string, n)
    return string:gsub(("."):rep(n), "%0 "):gsub("%s$", "")
end

----------------------------------
local atbash = function(text)
    local alnum_chars = (text or ""):gsub("%W", ""):lower()
    return alnum_chars:gsub("%l", encode_char)
end

return {
    decode = atbash,
    encode = function(text) return group_by(atbash(text), 5) end
}
