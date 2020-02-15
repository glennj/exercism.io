class School {
    private val population: MutableMap<Int, List<String>> = mutableMapOf()

    fun add(student: String, grade: Int) {
        population[grade] = grade(grade).plus(student)
    }

    fun grade(grade: Int): List<String> =
        population.getOrDefault(grade, emptyList()).sorted()

    fun roster(): List<String> =
        population.keys.sorted()
            .fold(emptyList()) {roster, grade ->
                roster.plus(grade(grade))
            }
}
