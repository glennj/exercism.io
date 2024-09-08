class Allergies
  Allergens = ['eggs', 'peanuts', 'shellfish', 'strawberries',
               'tomatoes', 'chocolate', 'pollen', 'cats']

  constructor: (score) ->
    @allergens = (Allergens[i] for i in [0...Allergens.length] when (score >> i) & 1 is 1)

  allergicTo: (candidate) -> candidate in @allergens
  list: -> @allergens

module.exports = Allergies
