object Raindrops {
    private val sounds = mapOf(3 to "Pling", 5 to "Plang", 7 to "Plong")

    fun convert(n: Int): String =
        with(sounds.filterKeys { n % it == 0 }) {
            when {
                this.isEmpty() -> n.toString()
                else -> this.values.joinToString("")
            }
        }

}
