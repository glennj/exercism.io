import sum from require 'lib/math'

single = (wanted, dice) ->
  sum [d for d in *dice when d == wanted]

yacht = (dice) ->
  table.sort dice
  if dice[1] == dice[5] then 50 else 0

full_house = (dice) ->
  table.sort dice
  return 0 if dice[1] == dice[5] -- yacht is not full
  return sum dice if dice[1] == dice[2] and dice[3] == dice[5]
  return sum dice if dice[1] == dice[3] and dice[4] == dice[5]
  0

four = (dice) ->
  table.sort dice
  if dice[1] == dice[4] or dice[2] == dice[5] then 4 * dice[3] else 0

straight = (wanted, dice) ->
  table.sort dice
  if wanted == table.concat dice then 30 else 0

{
  score: (category, dice) ->
    switch category
      when 'ones'            then single 1, dice
      when 'twos'            then single 2, dice
      when 'threes'          then single 3, dice
      when 'fours'           then single 4, dice
      when 'fives'           then single 5, dice
      when 'sixes'           then single 6, dice
      when 'full house'      then full_house dice
      when 'four of a kind'  then four dice
      when 'little straight' then straight '12345', dice
      when 'big straight'    then straight '23456', dice
      when 'yacht'           then yacht dice
      when 'choice'          then sum dice
}
