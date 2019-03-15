local PLANTS = {
    C = 'clover', G = 'grass', R = 'radishes', V = 'violets',
}

local STUDENTS = {
    'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred',
    'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry',
}

local parse
local plots 

local Garden = function(input)
    return setmetatable(parse(input:upper()), {
        __index = function(table, key)
            --[[ indexing the table within the __index
            --   metamethod can lead to an infinite loop:
            return table[key:lower()] or {}
            --]]
            local keyl = key:lower()
            for k,v in pairs(table) do
                if k == keyl then return v end
            end
            return {}
        end,
    })
end

parse = function(input)
    -- input must contain a newline, and
    -- the newline must be in the center
    local idx = input:find("\n")
    assert(idx and idx == 1 + #input//2, 'Malformed input')

    local garden = {}
    local i = 1
    for plot in plots(input:sub(1,idx-1), input:sub(idx+1)) do
        garden[STUDENTS[i]:lower()] = plot
        i = i + 1
    end
    return garden
end

plots = function(row1, row2)
    return coroutine.wrap(function()
        for i = 1, #row1, 2 do
            local patch = row1:sub(i,i+1) .. row2:sub(i,i+1)
            local plants = {}
            for p in patch:gmatch(".") do
                plants[#plants+1] = PLANTS[p] or "Unknown"
            end
            coroutine.yield(plants)
        end
    end)
end

return Garden
