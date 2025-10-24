-- A table-based solution.

local HighScores = {}

function HighScores:scores()
  return self._scores
end

function HighScores:latest()
  return self._scores[#self._scores]
end

function HighScores:personal_best()
  return self._sorted[1]
end

function HighScores:personal_top_three()
  return { table.unpack(self._sorted, 1, 3) }
end

return function(scores)
  local sorted = { table.unpack(scores) }             -- clone it
  table.sort(sorted, function(a, b) return a > b end) -- reverse sort

  local high_scores = { _scores = scores, _sorted = sorted }
  setmetatable(high_scores, { __index = HighScores })
  return high_scores
end
