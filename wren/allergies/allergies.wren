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

class Allergies {
  construct new(value) {
    _allergens = (0...ALLERGENS.count).reduce([]) {|list, i|
      if (((value >> i) & 1) == 1) {
        list.add(ALLERGENS[i])
      }
      return list
    }
  }

  list() {_allergens}

  allergicTo(name) {_allergens.contains(name)}
}
