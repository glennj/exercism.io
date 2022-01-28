class Dna(private val nucleotides: String) {
    private val valid = "ACGT"

    init {
        require(Regex("^[$valid]*$").matches(nucleotides))
    }

    val nucleotideCounts: Map<Char, Int>
        get() =
            with(nucleotides
                    .groupBy { it }
                    .mapValues { it.value.size }
                    .toMutableMap()
            ) {
                for (c in valid)
                    if (c !in this)
                        this[c] = 0
                this.toMap()
            }
}
