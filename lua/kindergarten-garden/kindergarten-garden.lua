local splitlines = require('pl.stringx').splitlines

local PLANTS = {
    C = 'clover', G = 'grass', R = 'radishes', V = 'violets',
}

local STUDENTS = {
    'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred',
    'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry',
}

local plots = function(row1, row2)
    return coroutine.wrap(function()
        for i = 1, #row1, 2 do
            local patch = row1:sub(i,i+1) .. row2:sub(i,i+1)
            local plants = {}
            for p in patch:gmatch(".") do
                plants[#plants+1] = PLANTS[p] or "Unknown"
            end
            coroutine.yield(i // 2 + 1, plants)
        end
    end)
end

function Garden(input)
    local garden = {}
    local rows = splitlines(input)
    for i, plot in plots(rows[1], rows[2]) do
        garden[STUDENTS[i]] = plot
    end

    garden.plants = function(name) return garden[name] end
    return garden
end

return Garden
