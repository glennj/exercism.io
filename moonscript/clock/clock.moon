normalize = (h, m) ->
  (h * 60 + m) % 1440 -- a day has 24 * 60 minutes

class Clock
  new: (argument) =>
    {:hour, :minute} = argument
    @_minutes = normalize hour, minute

  __tostring: =>
    string.format '%02d:%02d', @_minutes // 60, @_minutes % 60

  add: (minutes) =>
    @_minutes = normalize 0, @_minutes + minutes

  subtract: (minutes) =>
    @add -minutes

  equals: (a_clock) =>
    @_minutes == a_clock._minutes
