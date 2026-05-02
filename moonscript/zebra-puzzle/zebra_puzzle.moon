permute = require 'pl.permute'  -- https://luarocks.org/modules/tieske/penlight

rightOf = (a, b) -> a == b + 1
nextTo  = (a, b) -> math.abs(a - b) == 1

-- clue 1. There are five houses.
HOUSES = {1, 2, 3, 4, 5}
FIRST = 1
MIDDLE = 3

-- these are the functions we care about
local drinksWater, ownsZebra

solvePuzzle = ->
  for {red, green, ivory, yellow, blue} in permute.order_iter HOUSES
    -- clue 6. The green house is immediately to the right of the ivory house.
    continue if not rightOf green, ivory

    for {english, spanish, ukrainian, norwegian, japanese} in permute.order_iter HOUSES
      -- clue 2. The Englishman lives in the red house.
      -- clue 10. The Norwegian lives in the first house.
      -- clue 15. The Norwegian lives next to the blue house.
      continue if english != red
      continue if norwegian != FIRST
      continue if not nextTo norwegian, blue

      nationalities =
        [english]: 'EnglishMan'
        [spanish]: 'Spaniard'
        [ukrainian]: 'Ukrainian'
        [norwegian]: 'Norwegian'
        [japanese]: 'Japanese'

      for {coffee, tea, milk, orangeJuice, water} in permute.order_iter HOUSES
        -- clue 4. The person in the green house drinks coffee.
        -- clue 5. The Ukrainian drinks tea.
        -- clue 9. The person in the middle house drinks milk.
        continue if coffee != green
        continue if tea != ukrainian
        continue if milk != MIDDLE

        for {dancing, painting, reading, football, chess} in permute.order_iter HOUSES
          -- clue 8. The person in the yellow house is a painter.
          -- clue 13. The person who plays football drinks orange juice.
          -- clue 14. The Japanese person plays chess.
          continue if painting != yellow
          continue if football != orangeJuice
          continue if chess != japanese

          for {dog, snails, fox, horse, zebra} in permute.order_iter HOUSES
            -- clue 3. The Spaniard owns the dog.
            -- clue 7. The snail owner likes to go dancing.
            -- clue 11. The person who enjoys reading lives in the house next to the person with the fox.
            -- clue 12. The painter's house is next to the house with the horse.
            continue if dog != spanish
            continue if snails != dancing
            continue if not nextTo fox, reading
            continue if not nextTo horse, painting

            -- solved! now define the functions
            drinksWater = -> nationalities[water]
            ownsZebra   = -> nationalities[zebra]
            return true

assert solvePuzzle!, "No Solution Found!"

{ :drinksWater, :ownsZebra }
