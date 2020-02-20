class KindergartenGarden(val diagram: String) {

    init {
        require(diagram.all { it in "VRCG\n" })
    }

    private val rows = diagram.split('\n')
    init {
        require(rows.size == 2)
        require(rows[0].length == rows[1].length)
    }

    val students = listOf(
            "Alice", "Bob", "Charlie", "David", "Eve", "Fred",
            "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    )

    val plants = mapOf(
            'V' to "violets",
            'R' to "radishes",
            'C' to "clover",
            'G' to "grass"
    )

    val plots = rows[0].chunked(2)
                        .zip(rows[1].chunked(2))
                        .map { (r0, r1) ->
                            (r0 + r1).map { plants.getValue(it) }
                        }

    fun getPlantsOfStudent(student: String): List<String> {
        require(student in students)
        return plots[students.indexOf(student)]
    }
}
