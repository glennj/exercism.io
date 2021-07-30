// Stolen from the reference solution

import kotlin.math.absoluteValue

class ZebraPuzzle() {

    // houses
    private var one = 0
    private var two = 0
    private var three = 0
    private var four = 0
    private var five = 0

    // colours
    private var red = 0
    private var green = 0
    private var ivory = 0
    private var yellow = 0
    private var blue = 0

    // nationalities
    private var english = 0
    private var spanish = 0
    private var ukranian = 0
    private var norwegian = 0
    private var japanese = 0

    // pets
    private var dog = 0
    private var snails = 0
    private var fox = 0
    private var horse = 0
    private var zebra = 0

    // cigarettes
    private var oldGold = 0
    private var kools = 0
    private var chesterfields = 0
    private var luckyStrike = 0
    private var parliaments = 0

    // beverages
    private var coffee = 0
    private var tea = 0
    private var milk = 0
    private var orangeJuice = 0
    private var water = 0

    // These vars will be defined during the solving phase.
    private lateinit var nationalities: Map<Int, String>
    private lateinit var waterDrinker: String
    private lateinit var zebraOwner: String

    init { solve() }

    fun drinksWater(): String {
        return waterDrinker
    }

    fun ownsZebra(): String {
        return zebraOwner
    }

    companion object {
        private const val FIRST = 1
        private const val MIDDLE = 3

        // just return the permutations, rather than compute them.
        fun permutations(): List<List<Int>> {
            return listOf(
                listOf(1, 2, 3, 4, 5), listOf(1, 2, 3, 5, 4), listOf(1, 2, 4, 3, 5), listOf(1, 2, 4, 5, 3),
                listOf(1, 2, 5, 3, 4), listOf(1, 2, 5, 4, 3), listOf(1, 3, 2, 4, 5), listOf(1, 3, 2, 5, 4),
                listOf(1, 3, 4, 2, 5), listOf(1, 3, 4, 5, 2), listOf(1, 3, 5, 2, 4), listOf(1, 3, 5, 4, 2),
                listOf(1, 4, 2, 3, 5), listOf(1, 4, 2, 5, 3), listOf(1, 4, 3, 2, 5), listOf(1, 4, 3, 5, 2),
                listOf(1, 4, 5, 2, 3), listOf(1, 4, 5, 3, 2), listOf(1, 5, 2, 3, 4), listOf(1, 5, 2, 4, 3),
                listOf(1, 5, 3, 2, 4), listOf(1, 5, 3, 4, 2), listOf(1, 5, 4, 2, 3), listOf(1, 5, 4, 3, 2),
                listOf(2, 1, 3, 4, 5), listOf(2, 1, 3, 5, 4), listOf(2, 1, 4, 3, 5), listOf(2, 1, 4, 5, 3),
                listOf(2, 1, 5, 3, 4), listOf(2, 1, 5, 4, 3), listOf(2, 3, 1, 4, 5), listOf(2, 3, 1, 5, 4),
                listOf(2, 3, 4, 1, 5), listOf(2, 3, 4, 5, 1), listOf(2, 3, 5, 1, 4), listOf(2, 3, 5, 4, 1),
                listOf(2, 4, 1, 3, 5), listOf(2, 4, 1, 5, 3), listOf(2, 4, 3, 1, 5), listOf(2, 4, 3, 5, 1),
                listOf(2, 4, 5, 1, 3), listOf(2, 4, 5, 3, 1), listOf(2, 5, 1, 3, 4), listOf(2, 5, 1, 4, 3),
                listOf(2, 5, 3, 1, 4), listOf(2, 5, 3, 4, 1), listOf(2, 5, 4, 1, 3), listOf(2, 5, 4, 3, 1),
                listOf(3, 1, 2, 4, 5), listOf(3, 1, 2, 5, 4), listOf(3, 1, 4, 2, 5), listOf(3, 1, 4, 5, 2),
                listOf(3, 1, 5, 2, 4), listOf(3, 1, 5, 4, 2), listOf(3, 2, 1, 4, 5), listOf(3, 2, 1, 5, 4),
                listOf(3, 2, 4, 1, 5), listOf(3, 2, 4, 5, 1), listOf(3, 2, 5, 1, 4), listOf(3, 2, 5, 4, 1),
                listOf(3, 4, 1, 2, 5), listOf(3, 4, 1, 5, 2), listOf(3, 4, 2, 1, 5), listOf(3, 4, 2, 5, 1),
                listOf(3, 4, 5, 1, 2), listOf(3, 4, 5, 2, 1), listOf(3, 5, 1, 2, 4), listOf(3, 5, 1, 4, 2),
                listOf(3, 5, 2, 1, 4), listOf(3, 5, 2, 4, 1), listOf(3, 5, 4, 1, 2), listOf(3, 5, 4, 2, 1),
                listOf(4, 1, 2, 3, 5), listOf(4, 1, 2, 5, 3), listOf(4, 1, 3, 2, 5), listOf(4, 1, 3, 5, 2),
                listOf(4, 1, 5, 2, 3), listOf(4, 1, 5, 3, 2), listOf(4, 2, 1, 3, 5), listOf(4, 2, 1, 5, 3),
                listOf(4, 2, 3, 1, 5), listOf(4, 2, 3, 5, 1), listOf(4, 2, 5, 1, 3), listOf(4, 2, 5, 3, 1),
                listOf(4, 3, 1, 2, 5), listOf(4, 3, 1, 5, 2), listOf(4, 3, 2, 1, 5), listOf(4, 3, 2, 5, 1),
                listOf(4, 3, 5, 1, 2), listOf(4, 3, 5, 2, 1), listOf(4, 5, 1, 2, 3), listOf(4, 5, 1, 3, 2),
                listOf(4, 5, 2, 1, 3), listOf(4, 5, 2, 3, 1), listOf(4, 5, 3, 1, 2), listOf(4, 5, 3, 2, 1),
                listOf(5, 1, 2, 3, 4), listOf(5, 1, 2, 4, 3), listOf(5, 1, 3, 2, 4), listOf(5, 1, 3, 4, 2),
                listOf(5, 1, 4, 2, 3), listOf(5, 1, 4, 3, 2), listOf(5, 2, 1, 3, 4), listOf(5, 2, 1, 4, 3),
                listOf(5, 2, 3, 1, 4), listOf(5, 2, 3, 4, 1), listOf(5, 2, 4, 1, 3), listOf(5, 2, 4, 3, 1),
                listOf(5, 3, 1, 2, 4), listOf(5, 3, 1, 4, 2), listOf(5, 3, 2, 1, 4), listOf(5, 3, 2, 4, 1),
                listOf(5, 3, 4, 1, 2), listOf(5, 3, 4, 2, 1), listOf(5, 4, 1, 2, 3), listOf(5, 4, 1, 3, 2),
                listOf(5, 4, 2, 1, 3), listOf(5, 4, 2, 3, 1), listOf(5, 4, 3, 1, 2), listOf(5, 4, 3, 2, 1)
            )
        }

        // Int extension functions
        fun Int.toTheRightOf(other: Int): Boolean {
            return(this == other + 1)
        }
        fun Int.nextTo(other: Int): Boolean {
            return((this - other).absoluteValue == 1)
        }
    }

    private fun solve() {
        permutations().forEach { solveForColour(it) }
    }

    /* The clues, and the function that implements them:
       1. There are five houses.
       2. The Englishman lives in the red house. (solveForNationality)
       3. The Spaniard owns the dog. (solveForPets)
       4. Coffee is drunk in the green house. (solveForBeverages)
       5. The Ukrainian drinks tea. (solveForBeverages)
       6. The green house is immediately to the right of the ivory house.  (solveForColour)
       7. The Old Gold smoker owns snails. (solveForPets)
       8. Kools are smoked in the yellow house. (solveForSmokes)
       9. Milk is drunk in the middle house. (solveForBeverages)
       10.The Norwegian lives in the first house. (solveForNationality)
       11.The man who smokes Chesterfields lives in the house next to the man with the fox. (solveForPets)
       12.Kools are smoked in the house next to the house where the horse is kept.
       13.The Lucky Strike smoker drinks orange juice. (solveForSmokes)
       14.The Japanese smokes Parliaments. (solveForSmokes)
       15.The Norwegian lives next to the blue house. (solveForNationality)
     */

    private fun solveForColour(permutation: List<Int>) {
        red = permutation[0]
        green = permutation[1]
        ivory = permutation[2]
        yellow = permutation[3]
        blue = permutation[4]

        if (
            green.toTheRightOf(ivory)  // clue 6
        ) {
            permutations().forEach { solveForNationality(it) }
        }
    }

    private fun solveForNationality(permutation: List<Int>) {
        english = permutation[0]
        spanish = permutation[1]
        ukranian = permutation[2]
        norwegian = permutation[3]
        japanese = permutation[4]

        if (
            english == red              // clue 2
            && norwegian == FIRST       // clue 10
            && norwegian.nextTo(blue)  // clue 15
        ) {
            nationalities = mapOf(
                english to "Englishman",
                spanish to "Spaniard",
                ukranian to "Ukranian",
                norwegian to "Norwegian",
                japanese to "Japanese"
            )
            permutations().forEach { solveForBeverages(it) }
        }
    }

    private fun solveForBeverages(permutation: List<Int>) {
        coffee = permutation[0]
        tea = permutation[1]
        milk = permutation[2]
        orangeJuice = permutation[3]
        water = permutation[4]

        if (
            coffee == green             // clue 4
            && ukranian == tea          // clue 5
            && milk == MIDDLE           // clue 9
        ) {
            permutations().forEach { solveForSmokes(it) }
        }
    }

    private fun solveForSmokes(permutation: List<Int>) {
        oldGold = permutation[0]
        kools = permutation[1]
        chesterfields = permutation[2]
        luckyStrike = permutation[3]
        parliaments = permutation[4]

        if (
            kools == yellow                 // clue 8
            && luckyStrike == orangeJuice   // clue 13
            && japanese == parliaments      // clue 14
        ) {
            permutations().forEach { solveForPets(it) }
        }
    }

    private fun solveForPets(permutation: List<Int>) {
        dog = permutation[0]
        snails = permutation[1]
        fox = permutation[2]
        horse = permutation[3]
        zebra = permutation[4]

        if (
            spanish == dog                  // clue 3
            && oldGold == snails            // clue 7
            && chesterfields.nextTo(fox)    // clue 11
            && kools.nextTo(horse)          // clue 12
        ) {
            waterDrinker = nationalities[water].toString()
            zebraOwner = nationalities[zebra].toString()
        }
    }
}
