object BeerSong {
    private const val w = "on the wall"

    fun verses(startBottles: Int, takeDown: Int) =
        startBottles.downTo(takeDown).joinToString("\n") {
            val b = bottles(it)
            "$b $w, $b.\n".capitalize() +
            "${action(it)}, ${bottles(it - 1)} $w.\n"
        }

    private fun bottles(n: Int): String {
        val num = mapOf(-1 to "99", 0 to "no more")[n] ?: n.toString()
        return "$num bottle${if (n == 1) "" else "s"} of beer"
    }

    private fun action(n: Int): String = when (n) {
        0 -> "Go to the store and buy some more"
        else -> "Take ${if (n == 1) "it" else "one"} down and pass it around"
    }
}
