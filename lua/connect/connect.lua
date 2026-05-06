local split = require('pl.stringx').split
local clone = require('pl.tablex').deepcopy

-- It's simpler to code the "is_win" function to seek from top to bottom,
-- For X, we'll transpose the board to avoid seeking from left to right.
local function transpose(m)
  local t = {}
  for col = 1, #m[1] do
    local newrow = {}
    for row = 1, #m do
      table.insert(newrow, m[row][col])
    end
    table.insert(t, newrow)
  end
  return t
end

local Deltas = {{-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}}

local function neighbours(m, r, c, player)
  local height, width = #m, #m[1]
  local inrange = function(rr, cc)
    return 1 <= rr and rr <= height and 1 <= cc and cc <= width
  end
  local ns = {}
  for _, delta in ipairs(Deltas) do
    local dr, dc = delta[1], delta[2]
    if inrange(r + dr, c + dc) and player == m[r + dr][c + dc] then
      table.insert(ns, {r + dr, c + dc})
    end
  end
  return ns
end

local function is_win(player, m)
  local stack = {}
  local push = function(row, col)
    table.insert(stack, {row, col})
    m[row][col] = 'seen'
  end
  
  for col = 1, #m[1] do
    if m[1][col] == player then
      push(1, col)
    end
  end
  
  if #stack == 0 then return false end
  if #m == 1 then return true end

  -- loop until we run out of neighbours, or we reach the bottom
  while #stack > 0 do
    local coord = table.remove(stack)
    local r, c = coord[1], coord[2]
    for _, coord in ipairs(neighbours(m, r, c, player)) do
      local row, col = coord[1], coord[2]
      if row == #m then
        return true
      end
      push(row, col)
    end
  end
  return false
end


return {
  winner = function(board)
    matrix = {}
    for _, row in ipairs(board) do
      table.insert(matrix, split(row))
    end

    if is_win('O', clone(matrix)) then
      return 'O'
    elseif is_win('X', transpose(matrix)) then
      return 'X'
    else
      return ''
    end
  end
}
