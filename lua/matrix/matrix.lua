local Split = require("split")      -- https://luarocks.org/modules/telemachus/split
                                    -- luarocks install split
local split = Split.split
local each  = Split.each

local map = function (f, a)
    local result = {}
    for i, v in ipairs(a) do
        table.insert(result, f(v))
    end
    return result
end

local Matrix = function(string)
    local rows = {}
    for line in each(string, "\n") do
        table.insert(rows, map(tonumber, split(line)))
    end

    -- transpose the rows
    -- asssume each row has the same length
    local columns = {}
    for c,_ in ipairs(rows[1]) do
        columns[c] = {}
        for r,_ in ipairs(rows) do
            table.insert(columns[c], rows[r][c])
        end
    end

    local row = function(n)
        assert(1 <= n and n <= #rows)
        return rows[n]
    end

    local column = function(n)
        assert(1 <= n and n <= #columns)
        return columns[n]
    end

    return { row = row, column = column }
end

return Matrix
