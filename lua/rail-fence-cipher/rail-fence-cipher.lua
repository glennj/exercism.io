-- reimplementation based on
-- https://exercism.io/tracks/python/exercises/rail-fence-cipher/solutions/8d7425bdbb844c5e9416015cd7eb3daa

local railed_indices

local encode = function(text, n)
    if n == 1 or n >= #text then
        return text
    end
    local indices = railed_indices(text, n)
    local encoded = {}
    for i = 1, #indices do
        encoded[#encoded+1] = text:sub(indices[i], indices[i])
    end
    return table.concat(encoded)
end

local decode = function(text, n)
    if n == 1 or n >= #text then
        return text
    end
    local indices = railed_indices(text, n)
    -- a Schwartzian transform
    local a = {}
    for i = 1, #text do
        a[#a+1] = {i = indices[i], c = text:sub(i, i)}
    end
    table.sort(a, function(a, b) return a.i < b.i end)
    local decoded = {}
    for i = 1, #a do
        decoded[#decoded+1] = a[i].c
    end
    return table.concat(decoded)
end

---------------------------------------------------
-- Generate the pattern of rails.
-- Ex: #text = 10 and n = 3, we return:
--  {1, 2, 3, 2, 1, 2, 3, 2, 1, 2}
local rail_pattern = function(text, n)
    local cycle = {}
    for i = 1, n       do cycle[#cycle+1] = i end
    for i = n-1, 2, -1 do cycle[#cycle+1] = i end
    -- cycle is {1, 2, 3, 2}
    -- repeat it until length is #text
    local i = 1
    while #cycle < #text do
        cycle[#cycle+1] = cycle[i]
        i = i % #cycle + 1
    end
    return cycle
end

--[[
 Generate the indices required for encoding.

 Ex. given text = 'HELLOWORLD' and n = 3, then rails are

   1 2 3 4 5 6 7 8 9 10
 1 H . . . O . . . L .
 2 . E . L . W . R . D
 3 . . L . . . O . . .

 The encoded value is "HOLELWRDLO"

 This function returns the array:
  {1, 5, 9, 2, 4, 6, 8, 10, 3, 7}
-- ]]
railed_indices = function(text, n)
    local rp = rail_pattern(text, n)
    local indices = {}
    for i = 1, #text do
        indices[i] = {i=i, rail=rp[i]}
    end
    table.sort(indices, function(a, b)
        return a.rail < b.rail or
            (a.rail == b.rail and a.i < b.i)
    end)
    local result = {}
    for i = 1, #indices do
        result[#result+1] = indices[i].i
    end
    return result
end


---------------------------------------------------
return {encode = encode, decode = decode}


---------------------------------------------------
--[=[
local RialFenceCipher = {}
RialFenceCipher.__index = RialFenceCipher

function RialFenceCipher:new(n)
    return setmetatable({nr = n}, self)
end

function RialFenceCipher:init_pointer()
    self.ptr = 1
    self.dir = 1
end

function RialFenceCipher:incr_pointer()
    self.ptr = self.ptr + self.dir
    if self.ptr < 1 or self.ptr > self.nr then
        -- too far
        self.dir = -self.dir
        self.ptr = self.ptr + 2 * self.dir
    end
end

function RialFenceCipher:encode(input)
    if self.nr == 1 or self.nr > #input then
        return input
    end
    local rails = {}
    self:init_pointer()
    for i = 1, #input do
        rails[self.ptr] = (rails[self.ptr] or '') .. input:sub(i,i)
        self:incr_pointer()
    end
    return table.concat(rails)
end

local string_shift = function (string, len)
    return string:sub(1, len), string:sub(len + 1)
end

function RialFenceCipher:decode(input)
    if self.nr == 1 or self.nr > #input then
        return input
    end
    local rails = self:partition_ciphertext(input)
    local plaintext = ''
    self:init_pointer()
    local char
    for i = 1, #input do
        char, rails[self.ptr] = string_shift(rails[self.ptr], 1)
        plaintext = plaintext .. char
        self:incr_pointer()
    end
    return plaintext
end

function RialFenceCipher:partition_ciphertext(ciphertext)
    local rail_lengths = self:partition_lengths(ciphertext)
    local rails = {}
    for i,len in ipairs(rail_lengths) do
        rails[i], ciphertext = string_shift(ciphertext, len)
    end
    return rails
end

function RialFenceCipher:partition_lengths(ciphertext)
    -- i. each journey down and up the rails consumes 2 * (num_rails - 1) characters
    -- ii. the "inner" rails consume twice as many as the top & bottom rails
    local base_len = #ciphertext // (2 * (self.nr - 1))
    local leftover = #ciphertext %  (2 * (self.nr - 1))
    local lengths = {}
    for i = 1, self.nr do
        lengths[i] = base_len * ((i == 1 or i == self.nr) and 1 or 2)
    end
    -- handle the leftovers, 1 char per rail starting from the top
    self:init_pointer()
    for i = 1, leftover do
        lengths[self.ptr] = lengths[self.ptr] + 1
        self:incr_pointer()
    end
    return lengths
end

return {
    encode = function(input, nrail)
        local rfc = RialFenceCipher:new(nrail)
        return rfc:encode(input)
    end,
    decode = function(input, nrail)
        local rfc = RialFenceCipher:new(nrail)
        return rfc:decode(input)
    end,
}
--]=]
