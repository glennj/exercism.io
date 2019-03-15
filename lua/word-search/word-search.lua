-----------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------

-- A "map" function that also passes the list index to the function
local mapi = function (list, func)
    local result = {}
    for i, elem in ipairs(list) do
        result[i] = func(elem, i)
    end
    return result
end

--  '123',    '147',
--  '456', -> '258',
--  '789'     '369'
local transpose = function(list)
    local result = {}
    for col = 1, #list[1] do
        result[col] = ""
        for row = 1, #list do
            result[col] = result[col] .. list[row]:sub(col,col)
        end
    end
    return result
end

--  '123',    '321',
--  '456', -> '654',
--  '789'     '987'
local reverse = function(board)
    return mapi(board, string.reverse)
end

--  '123',    '789',
--  '456', -> '456',
--  '789'     '123'
local invert = function(board)
    return mapi(board, function(_, i)
        return board[#board - i + 1] 
    end)
end 

--  '123',    top     '  123',
--  '456', -> skew -> ' 456 ',
--  '789'             '789  '
--
--  '123',   bottom   '123  ',
--  '456', -> skew -> ' 456 ',
--  '789'             '  789'
local skew = function(top_or_bottom, board) 
    local is_top = top_or_bottom == "top"
    return mapi(board, function(row, i)
        local sp1, sp2 = #board - i, i - 1
        return ("%s%s%s"):format(
            (" "):rep(is_top and sp1 or sp2),
            row,
            (" "):rep(is_top and sp2 or sp1)
        )
    end)
end

-----------------------------------------------------------------
-- the WordSearch object
--
-- A frustrating amount of very similar methods that are just
-- different enough to prevent much code reuse.
--
-- My approach here is to only use left-to-right searching to
-- actually find the word, and then for all other directions,
-- I transform the puzzle itself to search left-to-right and
-- then remap the found coordinates.
--
-- For example, searching for "159"
-- 
-- '123',    '  123',    '  7',
-- '456', => ' 456 ', => ' 48',
-- '789'     '789  '     '159',  << found left-to-right
--                       '26 ',
--                       '3  '
--
-- Then remap {{1,3},{3,3} to {{3,1},{3,3}} to {{1,1},{3,3}}
-----------------------------------------------------------------

return function(puzzle)
    local left2right = function(word, board)
        board = board or puzzle
        for y = 1, #board do
            for x = 1, #board[1] - #word + 1 do
                if word == board[y]:sub(x, x + #word - 1) then
                    return {x, y}, {x + #word - 1, y}
                end
            end
        end
    end

    local right2left = function(word)
        local board = reverse(puzzle)
        local first, last = left2right(word, board)
        if first then
            first = {#board[1] - first[1] + 1, first[2]}
            last  = {#board[1] - last[1]  + 1, last[2]}
            return first, last
        end
    end

    local top2bottom = function(word, board)
        board = transpose(board or puzzle)
        local first, last = left2right(word, board)
        if first then
            return {first[2], first[1]}, {last[2], last[1]}
        end
    end

    local bottom2top = function(word)
        local board = invert(puzzle)
        local first, last = top2bottom(word, board)
        if first then
            first = {first[1], #board - first[2] + 1}
            last  = {last[1],  #board - last[2]  + 1}
            return first, last
        end
    end

    local topleft2bottomright = function(word)
        board = skew("top", puzzle)
        local first, last = top2bottom(word, board)
        if first then
            first = {first[1] - (#board - first[2]), first[2]}
            last  = {last[1]  - (#board - last[2]),  last[2]}
            return first, last
        end
    end

    local bottomleft2topright = function(word)
        local board = invert(skew("top", puzzle))
        local first, last = top2bottom(word, board)
        if first then
            first = {first[1] - (first[2] - 1), #board - first[2] + 1}
            last  = {last[1]  - (last[2] - 1),  #board - last[2]  + 1}
            return first, last
        end
    end

    local topright2bottomleft = function(word)
        board = skew("bottom", puzzle)
        local first, last = top2bottom(word, board)
        if first then
            first = {first[1] - (first[2] - 1), first[2]}
            last  = {last[1]  - (last[2]  - 1), last[2]}
            return first, last
        end
    end

    local bottomright2topleft = function(word)
        local board = invert(skew("bottom", puzzle))
        local first, last = top2bottom(word, board)
        if first then
            first = {first[1] - (#board - first[2]), #board - first[2] + 1}
            last  = {last[1]  - (#board - last[2]),  #board - last[2]  + 1}
            return first, last
        end
    end

    return {
        find = function(word)
            for _,search_func in ipairs({
                    left2right, 
                    right2left,
                    top2bottom, 
                    bottom2top,
                    topleft2bottomright, 
                    bottomleft2topright,
                    topright2bottomleft, 
                    bottomright2topleft,
            })
            do
                first, last = search_func(word)
                if first then return first, last end
            end
        end,
    }
end
