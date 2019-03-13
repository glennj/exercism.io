local extract_letters
local solves_it
-- Thanks to ryanplusplus for the combinations code
local combinations
local permutations

local solve = function(puzzle)
    -- scan puzzle for potentially dangerous code:
    -- only allow "arithmetic" characters and letters
    assert(puzzle:find("^[%u%d%s=+*/^-]+$"), 'Puzzle rejected')

    local letters = extract_letters(puzzle)

    for c in combinations({0,1,2,3,4,5,6,7,8,9}, #letters) do
        for p in permutations(c) do

            local map = {}
            for i = 1, #letters do
                map[letters[i]] = p[i]
            end

            if solves_it(puzzle, map) then
                return map
            end
        end
    end
end

extract_letters = function(puzzle)
    -- store the characters in a set for uniqueness
    local chars = {}
    for c in puzzle:gmatch("%u") do
        chars[c] = true
    end

    -- then extract the characters into a list
    local letters = {}
    for c,_ in pairs(chars) do
        letters[#letters+1] = c
    end

    return letters
end

solves_it = function(puzzle, map)
    local expr = puzzle:gsub("%u", map)

    if expr:find("%f[%d]0") then
        -- no leading zeroes allowed
        return false
    end

    -- eval a string of code that is expected to produce a boolean value
    local f = load('return ' .. expr)
    return f()
end


------------------------------------------------
-- liberated from ryanplusplus
-- https://exercism.io/tracks/lua/exercises/alphametics/solutions/9879a0d5dc374a08bf93032713a7fb6c 
local function generate_combinations(a, n, offset)
  if n > (#a - offset) then
    return
  end

  if n == 0 then
    coroutine.yield({})
    return
  end

  for v in combinations(a, n - 1, offset + 1) do
    table.insert(v, 1, a[1 + offset])
    coroutine.yield(v)
  end

  for v in combinations(a, n, offset + 1) do
    coroutine.yield(v)
  end
end

function combinations(a, n, offset)
  return coroutine.wrap(function() generate_combinations(a, n, offset or 0) end)
end

local function generate_permutations(a, n)
  if n == 0 then
    coroutine.yield(a)
  else
    for i = 1, n do
      a[n], a[i] = a[i], a[n]
      generate_permutations(a, n - 1)
      a[n], a[i] = a[i], a[n]
    end
  end
end

function permutations(a)
  return coroutine.wrap(function () generate_permutations(a, #a) end)
end
------------------------------------------------

return { solve = solve }
