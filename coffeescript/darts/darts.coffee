class Darts
  @score: (x, y) ->
    distance = Math.hypot x, y
    switch
      when distance > 10 then 0
      when distance > 5 then 1
      when distance > 1 then 5
      else 10

module.exports = Darts
