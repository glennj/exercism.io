local time = require('./time')
local list_ops = require('./list_ops')

local Stopwatch = {}
Stopwatch.__index = Stopwatch

function Stopwatch:new()
  local obj = {}
  setmetatable(obj, self)
  obj:reset(true)
  return obj
end

function Stopwatch:reset(is_init)
  assert(is_init or self:is_stopped(), 'cannot reset a stopwatch that is not stopped')
  self._state = 'ready'
  self._current = 0
  self._laps = {}
  -- I'm choosing to not have a `total` property. Although that's
  -- a simpler implementation, storing the laps in seconds does provide
  -- the raw data for calculating the total. But mainly I get to play with
  -- some functional programming.
end

function Stopwatch:state() return self._state end
function Stopwatch:is_running() return self._state == 'running' end
function Stopwatch:is_stopped() return self._state == 'stopped' end

function Stopwatch:start()
  assert(not self:is_running(), 'cannot start an already running stopwatch')
  self._state = 'running'
end

function Stopwatch:stop()
  assert(self:is_running(), 'cannot stop a stopwatch that is not running')
  self._state = 'stopped'
end

function Stopwatch:advance_time(timestamp)
  if self:is_running() then
    self._current = self._current + time.parse(timestamp)
  end
end

function Stopwatch:total()
  local total_seconds = self._current + list_ops.sum(self._laps)
  return time.format(total_seconds)
end

function Stopwatch:lap()
  assert(self:is_running(), 'cannot lap a stopwatch that is not running')
  table.insert(self._laps, self._current)
  self._current = 0
end

function Stopwatch:current_lap()   return time.format(self._current) end
function Stopwatch:previous_laps() return list_ops.map(self._laps, time.format) end

return Stopwatch
