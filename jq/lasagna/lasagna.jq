# The input will be null or an object that _may_ contain keys
#   actual_minutes_in_oven,
#   number_of_layers
#
# If the needed key is missing, use a default value:
#   zero minutes in oven,
#   one layer.
#
# Task: output a JSON object with keys:

40 as $expected_minutes_in_oven
| 2 as $prep_mins_per_layer
| (.actual_minutes_in_oven // 0) as $actual_mins
| ($expected_minutes_in_oven - $actual_mins) as $remaining_minutes_in_oven
| ((.number_of_layers // 1) * $prep_mins_per_layer) as $preparation_time
| {
    $expected_minutes_in_oven,
    $remaining_minutes_in_oven,
    $preparation_time,
    total_time: ($preparation_time + $actual_mins)
  }
