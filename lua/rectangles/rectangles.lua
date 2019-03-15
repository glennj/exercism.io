local VERTEX = "+"
local LINE = {      -- patterns for valid line characters
  vert  = "^[+|]+$",
  horiz = "^[+-]+$"
}

local find_vertices
local count_using
local is_line
local pop
local filter

local count = function(grid)
  local vertices = find_vertices(grid)
  if not next(vertices) then
    return 0
  end

  local sum = 0
  for y = 1, #grid do
    local top_vertices = filter(vertices, function(v) return v.y == y end)
    while #top_vertices >= 2 do
      local topR = pop(top_vertices)
      for _,topL in ipairs(top_vertices) do
        sum = sum + count_using(topL, topR, vertices, grid)
      end
    end
  end
  return sum
end

find_vertices = function (grid)
  local vertices = {}
  for y = 1, #grid do
    for x = 1, #grid[1] do
      if grid[y]:sub(x,x) == VERTEX then
        vertices[#vertices+1] = {x=x, y=y}
      end
    end
  end
  return vertices
end

count_using = function(topL, topR, vertices, grid)
  local sum = 0
  local left_vertices = filter(vertices, function(v)
    return v.x == topL.x and v.y > topL.y
  end)
  for _,bottomL in ipairs(left_vertices) do
    local bottomR = filter(vertices, function(v)
      return v.x == topR.x and v.y == bottomL.y
    end)
    if #bottomR == 1 then
      if    is_line(topL,       topR,       grid)
        and is_line(topR,       bottomR[1], grid)
        and is_line(bottomR[1], bottomL,    grid)
        and is_line(bottomL,    topL,       grid)
      then
        sum = sum + 1
      end
    end
  end
  return sum
end

is_line = function(p1, p2, grid)
  local line = ""
  local patt
  if p1.x == p2.x then
    patt = LINE.vert
    for y = math.min(p1.y, p2.y), math.max(p1.y, p2.y) do
      line = line .. grid[y]:sub(p1.x, p1.x)
    end
  else
    patt = LINE.horiz
    line = grid[p1.y]:sub(math.min(p1.x, p2.x), math.max(p1.x, p2.x))
  end
  return line:match(patt)
end

pop = function(list)
  local value = list[#list]
  list[#list] = nil
  return value
end

filter = function(list, func)
  local result = {}
  for _,elem in ipairs(list) do
    if func(elem) then
      result[#result+1] = elem
    end
  end
  return result
end

return { count = count }
