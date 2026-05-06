local function parse(input)
    local matrix = {}
    for _, line in ipairs(input) do
        local row = {}
        for char in line:gmatch('.') do
            row[#row + 1] = char
        end
        matrix[#matrix + 1] = row
    end
    return matrix
end

local function match_direction(m, r, c, dr, dc, word)
    local letters = ""
    for i = 0, #word - 1 do
        local rr = r + i * dr
        local cc = c + i * dc
        if rr < 1 or rr > #m or cc < 1 or cc > #m[rr] then
            break
        end
        letters = letters .. m[rr][cc]
    end
    if letters == word then
        return {
            start = {c, r},
            ['end'] = {c + (#word - 1) * dc, r + (#word - 1) * dr}
        }
    end
    return nil
end

local DIRECTIONS = {{0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}, {-1, -1}, {-1, 0}, {-1, 1}}

local function find(m, word)
    local len = #word
    for r = 1, #m do
        for c = 1, #m[1] do
            if m[r][c] == word:sub(1,1) then
                for _, dir in ipairs(DIRECTIONS) do
                    local result = match_direction(m, r, c, dir[1], dir[2], word)
                    if result then
                        return result
                    end
                end
            end
        end
    end
    return nil
end

return function(input)
    local matrix = parse(input)
    return {
        search = function(words)
            local found = {}
            for _, word in ipairs(words) do
                local result = find(matrix, word)
                if result then
                    found[word] = result
                end
            end
            return found
        end
    }
end
