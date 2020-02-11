import kotlin.random.Random

class Robot {

    companion object Names {
        private var names = mutableListOf<String>()

        // Generate the complete list of possible names and shuffle it.
        // The next name is then just the next one in the list.
        init {
            var a = 'A'
            var b = 'A'
            var c = 0
            while (a <= 'Z') {
                names.add(String.format("%c%c%03d", a, b, c))
                c++
                if (c > 999) { c = 0;   b++ }
                if (b > 'Z') { b = 'A'; a++ }
            }
            names.shuffle()
        }

        private var index = 0

        private fun nextName(): String {
            require(index < names.size) { "No more names" }
            return names[index++]
        }
    }

    var name: String = ""
        private set

    fun reset() {
        name = nextName()
    }

    init { reset() }
}

/*
 * Randomly generate a new name for each reset request.
 * This will take a long time as the names get closer to running out.
 *
class Robot {

    companion object Names {
        val names: MutableSet<String> = mutableSetOf()
    }

    var name: String = ""
        private set

    fun reset() {
        while (true) {
            require(Names.names.size < 26 * 26 * 1000) { "All names have been used." }

            name = String.format(
                    "%c%c%03d",
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[Random.nextInt(26)],
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[Random.nextInt(26)],
                    Random.nextInt(1000)
            )

            if (!Names.names.contains(name)) {
                Names.names.add(name)
                break
            }
        }
    }

    init { reset() }
}
*/
