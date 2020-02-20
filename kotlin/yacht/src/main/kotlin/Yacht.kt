import YachtCategory.*

object Yacht {

    fun solve(category: YachtCategory, vararg dices: Int): Int {
        val grouped = dices.toList().groupingBy {it}.eachCount()

        fun scoreFor(die: Int): Int = grouped.getOrDefault(die, 0) * die

        fun fullHouse(): Int = if (grouped.values.toSet() == setOf(2, 3)) dices.sum() else 0

        fun straight(cmp: IntRange): Int = if (dices.sorted() == cmp.toList()) 30 else 0

        fun fourOfAKind(): Int = 4 * (
                grouped.filter { it.value in listOf(4, 5) }
                       .map { it.key }
                       .firstOrNull() ?: 0
        )

        return when (category) {
            YACHT           -> if (grouped.size == 1) 50 else 0
            ONES            -> scoreFor(1)
            TWOS            -> scoreFor(2)
            THREES          -> scoreFor(3)
            FOURS           -> scoreFor(4)
            FIVES           -> scoreFor(5)
            SIXES           -> scoreFor(6)
            FULL_HOUSE      -> fullHouse()
            FOUR_OF_A_KIND  -> fourOfAKind()
            LITTLE_STRAIGHT -> straight(1..5)
            BIG_STRAIGHT    -> straight(2..6)
            CHOICE          -> dices.sum()
        }
    }

}
