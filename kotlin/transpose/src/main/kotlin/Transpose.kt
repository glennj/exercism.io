object Transpose {

    fun transpose(input: List<String>): List<String> {
        return when {
            input.isEmpty() -> input
            else -> with(simpleTranspose(input)) {
                // note to self, "this" is the result of the "with" expression

                // pad the transposed strings so that each line is
                // not shorter than the next line

                var currentWidth = this.last().trimEnd().length

                this.reversed()
                    .map {
                        val linePadded = it.trimEnd().padEnd(currentWidth)
                        currentWidth = linePadded.length
                        linePadded
                    }
                    .reversed()
            }
        }
    }

    private fun simpleTranspose(input: List<String>): List<String> {
        // Annoyingly kotlin.collections.max might be null
        //val max = input.map { it.length }.max() // Int?
        val max = input.fold(-1) { m, row -> kotlin.math.max(m, row.length) }

        // square the matrix and then tranpose it
        return with(input.map { it.padEnd(max) }) {
            this.first()
                .indices
                .map { i -> this.map { it[i] }.joinToString("") }
        }
    }
}
