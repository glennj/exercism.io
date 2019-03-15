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
