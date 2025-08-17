# Task 1: shopping list name
.name,

# Task 2: count the "required" ingredients
(.ingredients | length),

# Task 3: the amount of sugar
(.ingredients[] | select(.item == "sugar") | .amount.quantity),

# Task 4: map the listed substitutions
(
  ."optional ingredients" + .ingredients
  | map(select(has("substitute")) | {(.item): .substitute})
  | add
  | debug
)