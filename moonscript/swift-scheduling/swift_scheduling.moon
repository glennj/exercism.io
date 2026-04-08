parse_timestamp = (timestamp) ->
  parts = [tonumber m for m in timestamp\gmatch '(%d+)']
  {year, month, day, hour, min, sec} = parts
  os.date '*t', os.time {:year, :month, :day, :hour, :min, :sec}

first_workday = (year, month) ->
  t = os.date '*t', os.time {:year, :month, day: 1}
  switch t.wday
    when 1 then 2
    when 7 then 3
    else t.day

last_workday = (year, month) ->
  t = os.date '*t', os.time {:year, month: month + 1, day: 0}
  switch t.wday
    when 1 then t.day - 2
    when 7 then t.day - 1
    else t.day

-- ----------------------------------------------------------
-- these function mutate the input table
now = (t) ->
  t.hour += 2

asap = (t) ->
  if t.hour < 13
    t.hour = 17
  else
    t.day += 1
    t.hour = 13

eow = (t) ->
  switch t.wday
    when 2, 3, 4
      t.day += 6 - t.wday
      t.hour = 17
    when 5, 6
      t.day += 8 - t.wday
      t.hour = 20

month = (t, m) ->
  if t.month < m
    t.month = m
  else
    t.year += 1
    t.month = m
  t.day = first_workday t.year, t.month

quarter = (t, q) ->
  m = 3 * q
  if t.month < m
    t.month = m
  else
    t.year += 1
    t.month = m
  t.day = last_workday t.year, t.month

-- ----------------------------------------------------------
{
  delivery_date: (code, timestamp) ->
    t = with parse_timestamp timestamp
      .min = 0
      .sec = 0
    
    switch code
      when 'NOW' then now t
      when 'ASAP' then asap t
      when 'EOW' then eow t
      else
        m = code\match '(%d+)M'
        q = code\match 'Q([1234])'
        
        if m then month t, tonumber m
        else if q then quarter t, tonumber q
        else error 'Invalid code'

        t.hour = 8
        t.isdst = nil

    os.date '%Y-%m-%dT%H:%M:%S', os.time t
}
