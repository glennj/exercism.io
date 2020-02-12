object Transpose {

    fun transpose(input: List<String>): List<String> {
        var result = listOf<String>()

        if (!input.isEmpty()) {
            // Annoyingly kotlin.collections.max might be null
            //val max = input.map { it.length }.max() // Int?
            var max = input.fold(-1) { m, row -> kotlin.math.max(m, row.length) }

            // make the input matrix "square"
            val padded = input.map { it.padEnd(max) }

            // transpose it
            val transposed = padded.first().indices
                    .map { i -> padded
                            .map { it[i] }
                            .joinToString(separator = "")
                    }

            // pad the transposed strings so that each line is
            // not shorter than the next line
            var currentWidth = transposed.last().trimEnd().length
            result = transposed
                    .reversed()
                    .map {
                        val linePadded = it.trimEnd().padEnd(currentWidth)
                        currentWidth = linePadded.length
                        linePadded
                    }
                    .reversed()
        }

        return result
    }
}
