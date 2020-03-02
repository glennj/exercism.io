class ChainNotFoundException(msg: String) : RuntimeException(msg)

data class Domino(val left: Int, val right: Int) {
    fun reversed() = Domino(right, left)
    fun contains(dots: Int): Boolean = left == dots || right == dots
}

object Dominoes {

    fun formChain(vararg dominoes: Domino): List<Domino> = formChain(dominoes.toList())

    fun formChain(inputDominoes: List<Domino>): List<Domino> {
        if (inputDominoes.isEmpty()) return emptyList()

        inputDominoes.forEachIndexed { index, domino ->
            val remaining = inputDominoes.filterIndexed { i, _ -> i != index }
            val chain = chainFrom(listOf(domino), remaining) ?:
                        chainFrom(listOf(domino.reversed()), remaining)
            if (chain != null)
                return chain
        }

        throw ChainNotFoundException("")
    }

    private fun chainFrom(chain: List<Domino>, remaining: List<Domino>): List<Domino>? {
        if (remaining.isEmpty() && chain.first().left == chain.last().right)
            return chain

        val tail = chain.last().right
        remaining.forEachIndexed { index, domino ->
            if (domino.contains(tail)) {
                val d = if (domino.left == tail) domino else domino.reversed()
                val rest = remaining.filterIndexed { i, _ -> i != index }
                val result = chainFrom(chain + d, rest)
                if (result != null)
                    return result
            }
        }

        return null
    }
}


