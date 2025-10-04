const expected_bake_time = 60
const prep_time_per_layer = 2

preparation_time(layers) = layers * prep_time_per_layer

remaining_time(time_in_oven) = expected_bake_time - time_in_oven

total_working_time(layers, time_in_oven) = preparation_time(layers) + time_in_oven
