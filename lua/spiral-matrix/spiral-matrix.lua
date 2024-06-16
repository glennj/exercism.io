return function(size)
    local matrix = {}
    for x = 1,size do
        matrix[x] = {}
    end

    local x,  y  = 1, 1
    local dx, dy = 0, 1

    for i = 1, size * size do
        matrix[x][y] = i
        if  x + dx < 1 or x + dx > size or
            y + dy < 1 or y + dy > size or
            matrix[x + dx][y + dy] ~= nil
        then
            dx, dy = dy, -dx
        end
        x = x + dx
        y = y + dy
    end

    return matrix
end
