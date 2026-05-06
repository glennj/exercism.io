local permute = require'pl.permute'

local rightOf = function(a, b) return a == b + 1 end
local nextTo = function(a, b) return math.abs(a - b) == 1 end

-- clue 1. There are five houses.
local HOUSES = {1, 2, 3, 4, 5}
local FIRST = 1
local MIDDLE = 3

local drinksWater, ownsZebra -- we'll populate these anon

local solvePuzzle = function()
  for colours in permute.order_iter(HOUSES) do
    local red, green, ivory, yellow, blue = table.unpack(colours)
    -- clue 6. The green house is immediately to the right of the ivory house.
    if rightOf(green, ivory) then

      for countries in permute.order_iter(HOUSES) do
        local english, spanish, ukrainian, norwegian, japanese = table.unpack(countries)
        -- clue 2. The Englishman lives in the red house.
        -- clue 10. The Norwegian lives in the first house.
        -- clue 15. The Norwegian lives next to the blue house.
        if english == red and norwegian == FIRST and nextTo(norwegian, blue) then
          local nationalities = {
            [english] = 'EnglishMan',
            [spanish] = 'Spaniard',
            [ukrainian] = 'Ukrainian',
            [norwegian] = 'Norwegian',
            [japanese] = 'Japanese'
          }

          for beverages in permute.order_iter(HOUSES) do
            local coffee, tea, milk, orangeJuice, water = table.unpack(beverages)
            -- clue 4. The person in the green house drinks coffee.
            -- clue 5. The Ukrainian drinks tea.
            -- clue 9. The person in the middle house drinks milk.
            if coffee == green and tea == ukrainian and milk == MIDDLE then

              for hobbies in permute.order_iter(HOUSES) do
                local dancing, painting, reading, football, chess = table.unpack(hobbies)
                -- clue 8. The person in the yellow house is a painter.
                -- clue 13. The person who plays football drinks orange juice.
                -- clue 14. The Japanese person plays chess.
                if painting == yellow and football == orangeJuice and chess == japanese then

                  for pets in permute.order_iter(HOUSES) do
                    local dog, snails, fox, horse, zebra = table.unpack(pets)
                    -- clue 3. The Spaniard owns the dog.
                    -- clue 7. The snail owner likes to go dancing.
                    -- clue 11. The person who enjoys reading lives in the house next to the person with the fox.
                    -- clue 12. The painter's house is next to the house with the horse.
                    if dog == spanish and snails == dancing and nextTo(fox, reading) and nextTo(horse, painting) then
                      -- solved! now define the functions
                      drinksWater = function() return nationalities[water] end
                      ownsZebra   = function() return nationalities[zebra] end
                      return true
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
  return false
end

assert(solvePuzzle(), 'No solution found!')

return {
  drinks_water = drinksWater,
  owns_zebra = ownsZebra
}
