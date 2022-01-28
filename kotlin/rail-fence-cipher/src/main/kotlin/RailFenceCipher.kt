class RailFenceCipher(val numRails:Int) {

    private fun railSeq(): Sequence<Int> = sequence {
            var rail = 0
            var dir = 1
            while (true) {
                yield(rail)
                when {
                    rail == numRails - 1 && dir == 1 -> dir = -1
                    rail == 0 && dir == -1 -> dir = 1
                }
                rail += dir
            }
        }

    fun getEncryptedData(input: String): String {
        val rails = Array(numRails) {StringBuilder()}

        railSeq().take(input.length)
                .forEachIndexed { index, i ->
                    rails[i].append(input[index])
                }

        return rails.joinToString("") { it.toString() }
    }

    fun getDecryptedData(input: String): String {

        // determine the length of each rail
        val cycle = 2 * (numRails - 1)
        val cycles = input.length / cycle
        val railLengths = (0 until numRails).map {
            // inner rails consume 2 chars per cycle
            cycles * if (it in 1..numRails - 2) 2 else 1
        }.toMutableList()
        // account for the leftover characters
        railSeq().take(input.length - cycles * cycle)
                .forEach {railLengths[it] += 1}

        var start = 0
        val rails = railLengths.map {
            val rail = input.substring(start, start + it).toMutableList()
            start += it
            rail
        }

        //val result = StringBuilder(input.length)
        return railSeq()
                .take(input.length)
                .map { rails[it].unshift() }
                .joinToString("")
    }
}

fun <E> MutableList<E>.unshift(): E = this.removeAt(0)


/* *******************************************************
 * This is fewer lines of code, but I'm not happy with the
 * magic transforms passed by the encrypt/decrypt functions.
 *
class RailFenceCipherTake1(val numRails: Int) {

    private fun code(
            len: Int,
            putChar: (StringBuilder, Int, Int) -> Unit
    ): String
    {
        val result = with(StringBuilder(len)) {this.setLength(len); this}
        val cycleLen = 2 * (numRails - 1)

        var k = 0
        for (i in 0 until numRails) {
            for (j in 0..len / numRails) {
                var idx = i + j * cycleLen
                if (idx >= len) break
                putChar(result, idx, k++)
                // the middle rails consume 2 chars per cycle
                if (0 < i && i < numRails-1) {
                    idx = (cycleLen - i) + j * cycleLen
                    if (idx >= len) break
                    putChar(result, idx, k++)
                }
            }
        }
        return result.toString()
    }

    fun getEncryptedData(input: String): String =
        code(input.length) { sb, idx, k -> sb[k] = input[idx] }

    fun getDecryptedData(input: String): String =
        code(input.length) { sb, idx, k -> sb[idx] = input[k] }
}
 *
 */
