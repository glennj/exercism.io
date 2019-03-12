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

local joiner = string.char(034)

local concat_version = function (sublist, list)
    if #sublist == 0 then return true end
    local substring = joiner .. table.concat(sublist, joiner) .. joiner
    local string    = joiner .. table.concat(list, joiner) .. joiner
    return not not string:find(substring, 1, true)
end

-----------------------------------------------------

-- find the first index of the given element in the list,
-- *after* the "from" index
local index_of = function(list, element, from)
    from = from or 0
    local idx
    for i = from + 1, #list do
        if list[i] == element then 
            idx = i
            break
        end
    end
    return idx
end

local iterative_version = function (sublist, list)
    if #sublist == 0 then return true end

    local idx = index_of(list, sublist[1])
    while idx do
        local found = true
        for i = 2, #sublist do
            if sublist[i] ~= list[idx+i-1] then
                found = false
                break
            end
        end
        if found then return true end

        idx = index_of(list, sublist[1], idx)
    end
    return false
end

return true and iterative_version or concat_version
