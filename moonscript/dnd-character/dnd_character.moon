import fold from require 'moon'

sum = (xs) -> fold xs, (sum, x) -> sum + x
min = (xs) -> math.min table.unpack xs

d = math.random

attributes = {'strength', 'dexterity', 'constitution', 'intelligence', 'wisdom', 'charisma'}

modifier = (score) ->
  math.floor((score - 10) / 2)

ability = ->
  rolls = [d(6) for _ = 1,4]
  sum(rolls) - min(rolls)

{
  :modifier,
  :ability,
  character: ->
    player = {attr, ability! for attr in *attributes}
    player.hitpoints = 10 + modifier player.constitution
    player
}
