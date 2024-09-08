class Clock
  minutesPerDay = 24 * 60

  constructor: (hour = 0, minute = 0) ->
    @minutes = (hour * 60 + minute) %% minutesPerDay

  toString: () ->
    [@minutes // 60, @minutes % 60].map((n) -> n.toString().padStart(2, '0')).join(':')

  plus: (mins) ->
    @minutes = (@minutes + mins) %% minutesPerDay
    this

  minus: (mins) -> @plus(-mins)

  equals: (other) -> @minutes == other.minutes

      
module.exports = Clock
