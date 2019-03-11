local secrets = {
    {tonumber('00001', 2), 'wink'},
    {tonumber('00010', 2), 'double blink'},
    {tonumber('00100', 2), 'close your eyes'},
    {tonumber('01000', 2), 'jump'},
}
local reverse = tonumber('10000', 2)

local secret_handshake = function(code)
    local reversed = code & reverse > 0
    local handshake = {}
    for _, secret in ipairs(secrets) do
        local mask, action = table.unpack(secret)
        if code & mask > 0 then
            local pos = reversed and 1 or #handshake + 1
            table.insert(handshake, pos, action)
        end
    end
    return handshake
end

return secret_handshake
