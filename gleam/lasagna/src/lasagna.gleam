const prep_time_per_layer_in_minutes = 2

pub fn expected_minutes_in_oven() {
  40
}

pub fn remaining_minutes_in_oven(elapsed) {
  expected_minutes_in_oven() - elapsed
}

pub fn preparation_time_in_minutes(layers) {
  prep_time_per_layer_in_minutes * layers
}

pub fn total_time_in_minutes(layers, minutes_in_oven) {
  preparation_time_in_minutes(layers) + minutes_in_oven
}

pub fn alarm() {
  "Ding!"
}
