local reduce = function(list, accumulator, func)
  for _,elem in ipairs(list) do
    accumulator = func(elem, accumulator)
  end
  return accumulator
end

local sum = function(list)
  return reduce(list, 0, function(num, sum) 
    return num + sum 
  end)
end

local filter = function(list, func)
  return reduce(list, {}, function(elem, result)
    if func(elem) then result[#result+1] = elem end
    return result
  end)
end

------------------------------------------------------------
-- Bowline game class
--
-- This is the closure approach to OO
-- http://lua-users.org/wiki/ObjectOrientationClosureApproach

return function()
  local _score = 0
  local _frame = 1
  local _current = {}   -- rolls in the current frame
  local _bonuses = {}   -- strikes/spares we're still counting

  local add_score
  local handle_frame
  local handle_nth_frame
  local handle_tenth_frame
  local too_many
  local too_many_tenth

  local self = {
    roll = function(pins)
      assert(_frame <= 10, 'no rolls after game over')
      assert(0 <= pins and pins <= 10, 'invalid roll')
      assert(not too_many(pins), 'too many pins for frame')
      add_score(pins)
      handle_frame(pins)
    end,

    score = function()
      assert(_frame > 10, 'game not over yet')
      return _score
    end,
  }

  too_many = function(pins)
    if #_current == 0 then
      return false
    end
    if _frame == 10 then
      return too_many_tenth(pins)
    end
    return _current[1] + pins > 10
  end

  too_many_tenth = function(pins)
    local non_strikes = filter(_current, function(roll)
      return roll < 10
    end)
    if   #non_strikes == 0 or
        (#non_strikes == 2 and sum(non_strikes) == 10)
    then
      return false
    end
    return non_strikes[1] + pins > 10
  end

  add_score = function(pins)
    _score = _score + pins

    for i,b in pairs(_bonuses) do
      if b > 0 then
        _score = _score + pins
        _bonuses[i] = b - 1
      end
    end
  end

  handle_frame = function(pins)
    if _frame == 10 then
      handle_tenth_frame(pins)
    else
      handle_nth_frame(pins)
    end
  end

  -- recall I've already checked for too many pins
  handle_nth_frame = function(pins)
    if pins == 10 then
      table.insert(_bonuses, 2)       -- strike
      _frame = _frame + 1
    elseif #_current == 0 then
      table.insert(_current, pins)
    elseif #_current == 1 then
      if _current[1] + pins == 10 then
        table.insert(_bonuses, 1)     -- spare
      end
      _frame = _frame + 1
      _current = {}
    end
  end

  handle_tenth_frame = function(pins)
    table.insert(_current, pins)
    if  #_current == 3 or
       (#_current == 2 and sum(_current) < 10)
    then
      _frame = _frame + 1
    end
  end

  return self
end
