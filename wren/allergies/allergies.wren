var ALLERGENS = [
    "eggs",
    "peanuts",
    "shellfish",
    "strawberries",
    "tomatoes",
    "chocolate",
    "pollen",
    "cats",
]
//
class Allergies {
  construct new(value) {
    _allergens = (0...ALLERGENS.count).where {|i| ((value >> i) & 1) == 1}
                                      .map   {|i| ALLERGENS[i]}
  }
  list() {_allergens.toList}
  allergicTo(name) {_allergens.contains(name)}
}
