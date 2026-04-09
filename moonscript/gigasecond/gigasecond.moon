tz = require 'tz'

DATE = '(%d%d%d%d)-(%d%d)-(%d%d)'
TIME = '(%d%d):(%d%d):(%d%d)'

{
  add: (timestamp) ->
    -- parse the timestamp
    year, month, day, hour, min, sec = timestamp\match "^#{DATE}T#{TIME}$"
    if not year
      year, month, day = timestamp\match "^#{DATE}$"
      assert year, "cannot parse timestamp '#{timestamp}'"
      hour, min, sec = 0, 0, 0

    -- calculate the gigasecond anniversary
    utc_time = tz.time {:year, :month, :day, :hour, :min, :sec}, 'UTC'
    tz.date '%Y-%m-%dT%H:%M:%S', (utc_time + 1e9), 'UTC'
}
