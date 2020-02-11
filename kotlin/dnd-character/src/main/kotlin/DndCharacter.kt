import kotlin.math.floor
import kotlin.random.Random

class DndCharacter {

    companion object {
        fun d6() = Random.nextInt(6) + 1

        fun ability() = IntArray(4) { d6() }.sorted().drop(1).sum()

        fun modifier(score: Int) = floor((score - 10).toFloat().div(2)).toInt()
    }

    val strength: Int = ability()
    val dexterity: Int = ability()
    val constitution: Int = ability()
    val intelligence: Int = ability()
    val wisdom: Int = ability()
    val charisma: Int = ability()
    val hitpoints: Int = 10 + modifier(constitution)
}
