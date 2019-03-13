local validate
local count_bombs
local count_neighbours
local map
local split

local transform = function(board)
    validate(board)
    return count_bombs(board)
end

validate = function(board)
    local len = #board[1]
    for i = 1, #board do
        assert(len == #board[i])
        assert(board[i]:find("^.[* -]+.$"))
    end
end

count_bombs = function(board)
    local grid = map(board, split)
    for r = 2, #board - 1 do
        for c = 2, #board[1] - 1 do
            if grid[r][c] == ' ' then
                local n = count_neighbours(grid, r, c)
                grid[r][c] = n > 0 and n or ' '
            end
        end
    end
    return map(grid, table.concat)
end

local in_grid = function(grid, r, c)
    return 2 <= r and r <= #grid - 1 
       and 2 <= c and c <= #grid[r] - 1
end

count_neighbours = function(grid, r, c)
    local count = 0
    for dr = -1, 1 do
        for dc = -1, 1 do
            local rr = r + dr
            local cc = c + dc
            if in_grid(grid, rr, cc) and grid[rr][cc] == '*' then
                count = count + 1
            end
        end
    end
    return count
end

map = function(tbl, func)
    local result = {}
    for i = 1, #tbl do
        result[#result+1] = func(tbl[i])
    end
    return result
end

split = function(string)
    local chars = {}
    for i = 1, #string do
        chars[#chars+1] = string:sub(i,i)
    end
    return chars
end

return { transform = transform }
