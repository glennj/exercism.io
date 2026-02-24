allergens = {
  'eggs', 'peanuts', 'shellfish', 'strawberries',
  'tomatoes', 'chocolate', 'pollen', 'cats',
}


list = (score) ->
  is_allergic = (i) -> (score >> (i - 1)) & 1 == 1
  [a for i, a in ipairs allergens when is_allergic(i)]


allergic_to = (item, score) ->
  for a in *list(score)
    if a == item
      return true
  false


{:list, :allergic_to}
