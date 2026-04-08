DAYS = {'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'}

wday = (dayname) ->
  for i, d in ipairs DAYS
    return i if d == dayname
  error "unknown day name #{dayname}"

-- calling `os.time` *mutates* the table `t` as the components of t are normalized.
add_days = (t, n) ->
  t.day += n
  os.time t

{
  meetup: (input) ->
    t = {year: input.year, month: input.month, day: 1}
    os.time t

    -- find the first `dayofweek` day of the month
    while t.wday != wday input.dayofweek
      add_days t, 1 

    switch input.week
      when 'second' then add_days t, 7
      when 'third'  then add_days t, 14
      when 'fourth' then add_days t, 21
      when 'teenth' then add_days t, (t.day >= 6) and 7 or 14
      when 'last'
        -- get the last day of the input month (the zeroth day of _next_ month)
        last = {year: input.year, month: input.month + 1, day: 0}
        os.time last    
        while t.day <= last.day - 7
          add_days t, 7

    os.date '%Y-%m-%d', os.time t          
}
