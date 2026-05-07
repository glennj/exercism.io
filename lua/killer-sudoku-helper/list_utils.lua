local function size(t)
    local count = 0
    for _ in pairs(t) do count = count + 1 end
    return count
end

local function same(a, b)
    if type(a) ~= type(b) then
        return false
    elseif type(a) ~= "table" then
        return a == b
    elseif size(a) ~= size(b) then
        return false
    else
        for k, v in pairs(a) do
            if not b[k] or not same(v, b[k]) then
                return false
            end
        end
        return true
    end        
end
    
local function contains(list, item)
    for _, elem in ipairs(list) do
        if same(elem, item) then return true end
    end
    return false
end

local function copy(list)
    return { table.unpack(list) }
end

local function append(list, elem)
    table.insert(list, elem)
    return list
end

local function sort(list)
    table.sort(list)
    return list
end

return {
    append = append,
    contains = contains,
    copy = copy,
    same = same,
    size = size,
    sort = sort,
}
