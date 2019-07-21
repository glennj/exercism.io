class Allergies {
    static final ALLERGENS = [
        'eggs',
        'peanuts',
        'shellfish',
        'strawberries',
        'tomatoes',
        'chocolate',
        'pollen',
        'cats',
    ]

    final allergens = []

    Allergies(int score) {
        ALLERGENS.eachWithIndex { a, i ->
            if ((score & (1 << i)) != 0)
                allergens << a
        }
    }

    boolean allergicTo(allergen) {
        allergens.contains(allergen)
    }

    def list() {
        allergens
    }
}
