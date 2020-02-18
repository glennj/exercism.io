class Allergies(val score: Int) {

    fun getList(): List<Allergen> =
        Allergen.values().filter { isAllergicTo(it) }

    fun isAllergicTo(allergen: Allergen): Boolean =
        score and allergen.score != 0
}
