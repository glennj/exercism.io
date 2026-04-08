local parse_timestamp
parse_timestamp = function(timestamp)
  local parts
  do
    local _accum_0 = { }
    local _len_0 = 1
    for m in timestamp:gmatch('(%d+)') do
      _accum_0[_len_0] = tonumber(m)
      _len_0 = _len_0 + 1
    end
    parts = _accum_0
  end
  local year, month, day, hour, min, sec
  year, month, day, hour, min, sec = parts[1], parts[2], parts[3], parts[4], parts[5], parts[6]
  return os.date('*t', os.time({
    year = year,
    month = month,
    day = day,
    hour = hour,
    min = min,
    sec = sec
  }))
end
local first_workday
first_workday = function(year, month)
  local t = os.date('*t', os.time({
    year = year,
    month = month,
    day = 1
  }))
  local _exp_0 = t.wday
  if 1 == _exp_0 then
    return 2
  elseif 7 == _exp_0 then
    return 3
  else
    return t.day
  end
end
local last_workday
last_workday = function(year, month)
  local t = os.date('*t', os.time({
    year = year,
    month = month + 1,
    day = 0
  }))
  local _exp_0 = t.wday
  if 1 == _exp_0 then
    return t.day - 2
  elseif 7 == _exp_0 then
    return t.day - 1
  else
    return t.day
  end
end
local now
now = function(t)
  t.hour = t.hour + 2
end
local asap
asap = function(t)
  if t.hour < 13 then
    t.hour = 17
  else
    t.day = t.day + 1
    t.hour = 13
  end
end
local eow
eow = function(t)
  local _exp_0 = t.wday
  if 2 == _exp_0 or 3 == _exp_0 or 4 == _exp_0 then
    t.day = t.day + (6 - t.wday)
    t.hour = 17
  elseif 5 == _exp_0 or 6 == _exp_0 then
    t.day = t.day + (8 - t.wday)
    t.hour = 20
  end
end
local month
month = function(t, m)
  if t.month < m then
    t.month = m
  else
    t.year = t.year + 1
    t.month = m
  end
  t.day = first_workday(t.year, t.month)
end
local quarter
quarter = function(t, q)
  local m = 3 * q
  if t.month < m then
    t.month = m
  else
    t.year = t.year + 1
    t.month = m
  end
  t.day = last_workday(t.year, t.month)
end
return {
  delivery_date = function(code, timestamp)
    local t
    do
      local _with_0 = parse_timestamp(timestamp)
      _with_0.min = 0
      _with_0.sec = 0
      t = _with_0
    end
    local _exp_0 = code
    if 'NOW' == _exp_0 then
      now(t)
    elseif 'ASAP' == _exp_0 then
      asap(t)
    elseif 'EOW' == _exp_0 then
      eow(t)
    else
      local m = code:match('(%d+)M')
      local q = code:match('Q([1234])')
      if m then
        month(t, tonumber(m))
      else
        if q then
          quarter(t, tonumber(q))
        else
          error('Invalid code')
        end
      end
      t.hour = 8
      t.isdst = nil
    end
    return os.date('%Y-%m-%dT%H:%M:%S', os.time(t))
  end
}
