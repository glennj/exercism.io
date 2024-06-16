local function maximum_value(maximum_weight, items)
    local n = #items
    local m = {}
    for i = 0, n do
        m[i] = {}
    end

    for w = 0, maximum_weight do
        m[0][w] = 0
    end

    for i = 1, n do
        local wt = items[i].weight
        local val = items[i].value

        for w = 0, maximum_weight do
            if wt > w then
                m[i][w] = m[i-1][w]
            else
                m[i][w] = math.max(m[i-1][w], val + m[i-1][w-wt])
            end
        end
    end

    return m[n][maximum_weight]
end

return { maximum_value = maximum_value }
