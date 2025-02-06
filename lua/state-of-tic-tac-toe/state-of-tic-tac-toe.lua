local state_of

local gamestate = function (board)
    local win_x, nx = state_of(board, 'X')
    local win_o, no = state_of(board, 'O')

    assert(nx - no <= 1, 'X went twice')
    assert(nx - no >= 0, 'O started')
    assert(not (win_x and win_o), 'play continued after game ended')

    if win_x or win_o  then return 'win' end
    if nx + no == 9    then return 'draw' end
    return 'ongoing'
end

-- ------------------------------------------------------------
local win_masks = {
    0x007,  -- 3rd row      000 000 111
    0x038,  -- 2nd row      000 111 000
    0x1c0,  -- 1st row      111 000 000
    0x049,  -- 3rd column   001 001 001
    0x092,  -- 2nd column   010 010 010
    0x124,  -- 1st column   100 100 100
    0x111,  -- diagonal 1   100 010 001
    0x054,  -- diagonal 2   001 010 100
}

local is_win = function (value)
    for _, mask in ipairs(win_masks) do
        if value & mask == mask then
            return true
        end
    end
    return false
end

state_of = function (board, player)
    local value, count, offset = 0, 0, 9
    for _, row in ipairs(board) do
        for char in row:gmatch('.') do
            offset = offset - 1
            if char == player then
                value = value | (1 << offset)
                count = count + 1
            end
        end
    end
    return is_win(value), count
end
-- ------------------------------------------------------------

return { gamestate = gamestate }
