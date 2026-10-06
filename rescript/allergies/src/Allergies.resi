type allergen =
  | Cats
  | Pollen
  | Chocolate
  | Tomatoes
  | Strawberries
  | Shellfish
  | Peanuts
  | Eggs

let allergicTo: (allergen, int) => bool
let list: int => array<allergen>
