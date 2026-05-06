local dump = require('pl.pretty').dump

-----------------------------------------------------
-- Join the lists using a unlikely character, and use string
-- find to determine inclusion.
--
-- This character is also used by awk as the default value of
-- the SUBSEP variable, which is used to build array keys for 
-- pseudo-multidimensional arrays.
--
-- This version will be quick, but is susceptable to bad results
-- if any list element contains the special character.

local joiner = string.char(31)  -- the ascii US (unit separator) char

local concat_version = function(a, b)
    local sA = joiner .. table.concat(a, joiner) .. joiner
    local sB = joiner .. table.concat(b, joiner) .. joiner
    local result
    if #a == #b and sA == sB then
        result = 'equal'
    elseif #a < #b and #a == 0 or sB:find(sA, 1, true) then
        result = 'sublist'
    elseif #a > #b and #b == 0 or sA:find(sB, 1, true) then
        result = 'superlist'
    else
        result = 'unequal'
    end
    return result
end

-----------------------------------------------------
-- Iteratively:

-- is list b contained within list a
local contains = function(a, b)
    for i = 1, #a - #b + 1 do
        if a[i] == b[1] then
            local found = true
            for j = 2, #b do
                if a[i + j - 1] ~= b[j] then
                    found = false
                    break
                end
            end
            if found then return true end
        end
    end
    return false
end

local iterative_version = function (a, b)
    local result
    if #a == #b and contains(a, b) then
        result = 'equal'
    elseif #a < #b and contains(b, a) then
        result = 'sublist'
    elseif #a > #b and contains(a, b) then
        result = 'superlist'
    else
        result = 'unequal'
    end
    return result
end

return true and iterative_version or concat_version
