local FLOWER = '*'

local in_grid = function(grid, r, c)
    return 1 <= r and r <= #grid
       and 1 <= c and c <= #grid[r]
end

local count_neighbours = function(grid, r, c)
    local count = 0
    for dr = -1, 1 do
        for dc = -1, 1 do
            local rr = r + dr
            local cc = c + dc
            if in_grid(grid, rr, cc) and grid[rr][cc] == FLOWER then
                count = count + 1
            end
        end
    end
    return count
end

local map = function(tbl, func)
    local result = {}
    for i = 1, #tbl do
        result[#result+1] = func(tbl[i])
    end
    return result
end

local split = function(string)
    local chars = {}
    for i = 1, #string do
        chars[#chars+1] = string:sub(i,i)
    end
    return chars
end

-- ------------------------------------------------------------
return {
    annotate = function (field)
        local grid = map(field, split)
        for r = 1, #field do
            for c = 1, #field[1] do
                if grid[r][c] == ' ' then
                    local n = count_neighbours(grid, r, c)
                    grid[r][c] = n > 0 and n or ' '
                end
            end
        end
        return map(grid, table.concat)
    end
}
