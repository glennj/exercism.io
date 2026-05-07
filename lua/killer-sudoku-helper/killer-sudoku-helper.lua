local L = require('./list_utils')

local function combinations(sum, size, exclude)
    exclude = exclude or {}
    if size == 1 then
        if 1 <= sum and sum <= 9 and not L.contains(exclude, sum) then
            return {{sum}}
        else
            return {}
        end
    else
        local results = {}
        for n = 1, 9 do
            if not L.contains(exclude, n) then
                local newexclude = L.append(L.copy(exclude), n)
                for _, c in ipairs(combinations(sum - n, size - 1, newexclude)) do
                    local combination = L.sort(L.append(c, n))
                    if not L.contains(results, combination) then
                        table.insert(results, combination)
                    end
                end
            end
        end
        return results
    end
end

return { combinations = combinations }
