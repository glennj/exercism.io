local clone
local count_neighbours
local in_mtx

local tick = function(matrix)
    local next_matrix = clone(matrix)
    for r = 1, #matrix do
        for c = 1, #matrix[r] do
            local n = count_neighbours(matrix, r, c)
            if n == 3 then
                next_matrix[r][c] = 1
            elseif n ~= 2 then
                next_matrix[r][c] = 0
            end
        end
    end
    return next_matrix
end

clone = function(matrix)
    local result = {}
    for _, row in ipairs(matrix) do
        table.insert(result, { table.unpack(row) })
    end
    return result
end

count_neighbours = function(mtx, r, c)
    local count = 0
    for dr = -1, 1 do
        for dc = -1, 1 do
            if (dr ~= 0 or dc ~= 0) and in_mtx(mtx, r + dr, c + dc) then
                count = count + mtx[r + dr][c + dc]
            end
        end
    end
    return count
end

in_mtx = function(mtx, r, c)
    return 1 <= r and r <= #mtx
       and 1 <= c and c <= #mtx[r]
end

return { tick = tick }
