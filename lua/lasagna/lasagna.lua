  
local lasagna = {}

lasagna.oven_time = 40           -- minutes
lasagna.prep_time_per_layer = 2  -- minutes

-- returns the remaining minutes based on the actual minutes in the oven
function lasagna.remaining_oven_time(actual_minutes_in_oven)
  return lasagna.oven_time - actual_minutes_in_oven
end

-- calculates and returns the time needed to prepare the lasagna on the amount
-- of layers
function lasagna.preparation_time(number_of_layers)
  return lasagna.prep_time_per_layer * number_of_layers
end

-- calculates the time elapsed cooking the lasagna - including preparation time
-- and baking time.
function lasagna.elapsed_time(number_of_layers, actual_minutes_in_oven)
  return lasagna.preparation_time(number_of_layers) + actual_minutes_in_oven
end

return lasagna
