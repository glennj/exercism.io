local format_seconds, parse_duration

class Stopwatch
  new: =>
    @_state = 'stopped'
    @reset!

  reset: =>
    assert @isStopped!, 'cannot reset a stopwatch that is not stopped'
    @_total, @_current, @_laps, @_state = 0, 0, {}, 'ready'

  state:        => @_state
  previousLaps: => @_laps

  isRunning: => @_state == 'running'
  isStopped: => @_state == 'stopped'

  currentLap: => format_seconds @_current
  total:      => format_seconds @_total

  start: =>
    assert not @isRunning!, 'cannot start an already running stopwatch'
    @_state = 'running'

  stop: =>
    assert @isRunning!, 'cannot stop a stopwatch that is not running'
    @_state = 'stopped'

  lap: =>
    assert @isRunning!, 'cannot lap a stopwatch that is not running'
    table.insert @_laps, @currentLap!
    @_current = 0

  advanceTime: (duration) =>
    if @isRunning!
      seconds = parse_duration duration
      @_current += seconds
      @_total   += seconds

-- ------------------------------------------------------------
format_seconds = (seconds) ->
  secs = seconds % 60
  mins = (seconds // 60) % 60
  hrs  = seconds // (60 * 60)
  string.format '%02d:%02d:%02d', hrs, mins, secs

parse_duration = (duration) ->
  h, m, s = duration\match '^(%d+):(%d+):(%d+)$'
  assert h and m and s, "malformed duration: '#{duration}', expecting 'HH:MM:SS'"
  tonumber(h) * 3600 + tonumber(m) * 60 + tonumber(s)

-- ------------------------------------------------------------
{ :Stopwatch }
