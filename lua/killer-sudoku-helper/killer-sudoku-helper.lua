local function equals(a, b)
    if type(a) == "number" then
        return a == b
    elseif type(a) == "table" then
        if #a ~= #b then
            return false
        else
            for k, v in pairs(a) do
                if v ~= b[k] then
                    return false
                end
            end
            return true
        end
    end
end


local function contains(list, value)
    for _, v in ipairs(list) do
        if equals(v, value) then
            return true
        end
    end
    return false
end


local function combinations(sum, size, exclude)
    exclude = exclude or {}
    if size == 1 then
        if (1 <= sum and sum <= 9) and not contains(exclude, sum) then
            return {{ sum }}
        else
            return {}
        end
    else
        local results = {}
        for n = 1,9 do
            if not contains(exclude, n) then
                local sub_combinations = combinations(sum - n, size - 1, { table.unpack(exclude), n })
                for _, comb in ipairs(sub_combinations) do
                    local cage = { table.unpack(comb), n }
                    table.sort(cage)
                    if not contains(results, cage) then
                        table.insert(results, cage)
                    end
                end
            end
        end
        return results
    end
end

return { combinations = combinations }
