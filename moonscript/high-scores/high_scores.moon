-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
List = require 'pl.List'

class HighScores
  new: (scoreList) =>
    @s = List scoreList

  scores: =>
    @s

  latest: =>
    @s[@s\len!]

  personalBest: =>
    _, max = @s\minmax!
    max

  personalTopThree: =>
    @s\sorted('>')\slice(1, 3)
