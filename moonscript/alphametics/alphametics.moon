import fold from require 'moon'

-- test if a table contains a value
containsValue = (t, value) ->
  for k, v in pairs t
    return true if v == value
  false

-- --------------------------------------------------
-- The sum of last digit of each addend must correspond with the last digit of
-- the total.
-- This function returns 3 possible values:
--   nil,   if not all of the letters have been mapped
--   true,  if the last digit sum is valid
--   false, otherwise
lastDigitsValid = (mapping, letters) ->
  sum = 0
  for i, c in ipairs letters
    return nil if not mapping[c]
    sum += mapping[c] if i < #letters
  sum % 10 == mapping[letters[#letters]]

-- --------------------------------------------------
-- here, we prioritize solving the lastLetters first, as that's 
-- a main constraint. Then we order by frequency, most-used first.
sortUniqLetters = (puzzle, lastLetters) ->
  allCount = {}
  for char in puzzle\gmatch '%a'
    allCount[char] = (allCount[char] or 0) + 1

  lastCount = {}
  lastCount[c] = (lastCount[c] or 0) + 1 for c in *lastLetters

  rest = {c, n for c, n in pairs allCount when not lastCount[c]}

  orderKeys = (t) ->
    keys = [k for k, _ in pairs t]
    table.sort keys, (a, b) -> t[a] > t[b]
    keys

  ordered = {}
  table.insert ordered, c for c in *orderKeys lastCount
  table.insert ordered, c for c in *orderKeys rest
  ordered

-- --------------------------------------------------
solvesPuzzle = (puzzle, mapping) ->
  eqn = puzzle\gsub '%a', mapping
  load("return #{eqn}")!

-- --------------------------------------------------
{
  solve: (puzzle) ->
    words = [word for word in puzzle\gmatch '%a+']
    firstLetters = [word\sub(1, 1) for word in *words]
    lastLetters = [word\sub(-1) for word in *words]
    uniqLetters = sortUniqLetters puzzle, lastLetters

    helper = (mapping, idx) ->
      if idx > #uniqLetters
        -- we've mapped all the letters: do we have a solution?
        if solvesPuzzle puzzle, mapping
          return mapping
        else
          return nil

      letter = uniqLetters[idx]

      -- the first digit of an addend cannot be zero
      start = if containsValue firstLetters, letter then 1 else 0

      for digit = start, 9
        continue if containsValue mapping, digit

        mapping[letter] = digit

        -- test the constraint that the last digits add up correctly
        check = lastDigitsValid mapping, lastLetters 
        if check == true or check == nil

          -- recurse with current mapping to test next letter
          result = helper mapping, idx + 1
          if result then return result

        -- no solution for this mapping, try the next digit
        mapping[letter] = nil

      -- no solution for this mapping
      nil

    helper {}, 1
}
