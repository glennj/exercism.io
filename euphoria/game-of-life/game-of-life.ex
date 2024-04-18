include std/search.e

constant DEAD = 0, LIVE = 1

public function tick(sequence matrix)
    sequence next = {}
    for row = 1 to length(matrix) do
        next = append(next, {})
        for col = 1 to length(matrix[1]) do
            next[row] &= life(matrix, row, col)
        end for
    end for
    return next
end function

function life(sequence m, integer r, integer c)
    switch count(m, r, c) do
        case 2 then return m[r][c]
        case 3 then return LIVE
        case else   return DEAD
    end switch
end function

function count(sequence m, integer r, integer c)
    sequence deltas = {{-1, -1}, {-1, 0}, {-1, 1},
                       { 0, -1},          { 0, 1},
                       { 1, -1}, { 1, 0}, { 1, 1}}
    integer live_neighbours = 0
    for d = 1 to length(deltas) do
        integer dr = r + deltas[d][1]
        integer dc = c + deltas[d][2]
        if valid_indices(m, dr, dc) then
            live_neighbours += m[dr][dc]
        end if
    end for
    return live_neighbours
end function

function valid_indices(sequence m, integer r, integer c)
    return is_in_range(r, {1, length(m)}, "[]")
       and is_in_range(c, {1, length(m[1])}, "[]")
end function
