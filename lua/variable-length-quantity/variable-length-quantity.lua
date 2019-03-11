-- some constants
local SHIFT_AMT = 7
local MSB  = tonumber('10000000', 2)
local MASK = tonumber('01111111', 2)

local function decode(bytes)
    if bytes[#bytes] & MSB ~= 0 then
        error('incomplete byte sequence')
    end

    local values = {}
    local n = 0
    for i = 1, #bytes do
        local byte = bytes[i]
        n = (n << SHIFT_AMT) + (byte & MASK)
        if byte & MSB == 0 then
            table.insert(values, n)
            n = 0
        end
    end
    return values
end

local function encode(values)
    local bytes = {}
    for i = #values, 1, -1 do
        local val = values[i]
        local msb = 0
        repeat
            local byte = (val & MASK) | msb
            table.insert(bytes, 1, byte)
            msb = MSB
            val = val >> SHIFT_AMT
        until val == 0
    end
    return bytes
end

return { encode = encode, decode = decode }
