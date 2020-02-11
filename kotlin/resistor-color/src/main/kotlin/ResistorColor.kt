object ResistorColor {

    private val Colors = listOf("black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white")

    // TODO: error handling for invalid color
    fun colorCode(input: String): Int = Colors.indexOf(input)

    fun colors(): List<String> = Colors
}
