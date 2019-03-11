-- I'm on lua v5.1
local bit32 = require("bit32")
local _and    = bit32.band
local _or     = bit32.bor
local _lshift = bit32.lshift
local _rshift = bit32.rshift

-- some constants
local SHIFT_AMT = 7
local MSB  = 0x80       -- 0b1000_0000
local MASK = 0x7f       -- 0b0111_1111

local function decode(bytes)
    if _and(bytes[#bytes], MSB) ~= 0 then
        error('incomplete byte sequence')
    end

    local values = {}
    local n = 0
    for i = 1, #bytes do
        local byte = bytes[i]
        n = _lshift(n, SHIFT_AMT) + _and(byte, MASK)
        if _and(byte, MSB) == 0 then
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
            local byte = _or(_and(val, MASK), msb)
            table.insert(bytes, 1, byte)
            msb = MSB
            val = _rshift(val, SHIFT_AMT)
        until val == 0
    end
    return bytes
end

return { encode = encode, decode = decode }
