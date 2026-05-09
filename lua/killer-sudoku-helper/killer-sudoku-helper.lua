local Seq = require('./seq')

local function combinations(sum, size, exclude)
    exclude = Seq(exclude)
    if size == 1 then
        if 1 <= sum and sum <= 9 and not exclude:contains(sum) then
            return Seq({Seq({sum})})
        else
            return {}
        end
    else
        local results = Seq()
        for n = 1, 9 do
            if not exclude:contains(n) then
                for _, c in ipairs(combinations(sum - n, size - 1, exclude:append(n))) do
                    local combination = c:append(n):sorted()
                    if not results:contains(combination) then
                        results:append(combination)
                    end
                end
            end
        end
        return results
    end
end

return { combinations = combinations }
