local HighScores = {}
HighScores.__index = HighScores
setmetatable(HighScores, {
  __call = function(cls, ...) return cls:new(...) end,
})

function HighScores:new(values)
  assert(values)
  return setmetatable({_scores = values}, self)
end

function HighScores:scores()
  return self._scores
end

function HighScores:latest()
  return self._scores[#self._scores]
end

function HighScores:personal_best()
  if self._max == nil then
    self._max = math.max(table.unpack(self._scores))
  end
  return self._max
end

function HighScores:personal_top_three()
  if self._top3 == nil then
    local clone = { table.unpack(self._scores) }
    table.sort(clone, function(a, b) return a > b end) -- reverse sort
    self._top3 = { table.unpack(clone, 1, 3) }
  end
  return self._top3
end

return HighScores
