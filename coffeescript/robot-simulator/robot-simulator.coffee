class RobotSimulator
  rightTurns = "north": "east", "east": "south", "south": "west", "west": "north"
  leftTurns  = "north": "west", "east": "north", "south": "east", "west": "south"

  constructor: (@x, @y, @direction) ->

  move: (commands) ->
    for instruction in commands
      if instruction in "RLA"
        this[instruction]()

  R: () -> @direction = rightTurns[@direction]
  L: () -> @direction = leftTurns[@direction]
  A: () ->
    switch @direction
      when "north" then ++@y
      when "east"  then ++@x 
      when "south" then --@y
      when "west"  then --@x
      
module.exports = RobotSimulator
