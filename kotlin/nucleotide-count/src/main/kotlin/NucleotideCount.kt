class Dna(private val nucleotides: String) {
    private val valid = "ACGT"

    init {
        //require(Regex("^[$valid]*$").matches(nucleotides))
        require(nucleotides.all { it in valid })
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

/*
https://exercism.io/tracks/kotlin/exercises/nucleotide-count/solutions/00a3ab11bd1b4ae49bc05b6a68c26b33
class Dna(string: String) {
    init { require(string.matches("[ACGT]*".toRegex())) { "Only A, C, G and T allowed in DNA string." } }

    val nucleotideCounts = ("ACGT" + string).groupBy { it }.mapValues { it.value.size - 1 }
}


https://exercism.io/tracks/kotlin/exercises/nucleotide-count/solutions/62aa2552832d427898c96292c4d27296

class Dna(  val sequence : String ) {
  init {
    val nucleotides = "ACGT"
    require(sequence.all { it in nucleotides })
  }
  val nucleotideCounts = mapOf('A' to 0, 'C' to 0, 'G' to 0, 'T' to 0) + sequence.groupingBy{ it }.eachCount()
}

 */
