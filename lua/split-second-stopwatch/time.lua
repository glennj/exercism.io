local function format(time)
  local hours = time // 3600
  local mins = (time % 3600) // 60
  local secs = time % 60
  return string.format('%02d:%02d:%02d', hours, mins, secs)
  -- `return os.date('!%T', time)` is simpler, but it can't handle
  -- durations of more than 24 hours.
end

local function parse(timestamp)
  local hours, minutes, seconds = timestamp:match('^(%d%d+):(%d%d):(%d%d)$')
  assert(hours, 'Incorrect timestamp format: expecting HH:MM:SS')
  return (tonumber(hours) * 60 + tonumber(minutes)) * 60 + tonumber(seconds)
end

return {
  format = format,
  parse = parse,
}
