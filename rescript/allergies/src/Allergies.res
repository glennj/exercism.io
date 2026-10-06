type allergen =
  | Cats
  | Pollen
  | Chocolate
  | Tomatoes
  | Strawberries
  | Shellfish
  | Peanuts
  | Eggs

let allergens = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

let list = n => Array.filterWithIndex(allergens, (_, i) => (n >> i &&& 1) == 1)

let allergicTo = (allergy, n) => list(n)->Array.includes(allergy)
