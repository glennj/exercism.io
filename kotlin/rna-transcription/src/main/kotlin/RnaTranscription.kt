fun transcribeToRna(dna: String): String = dna.map {
        when(it) {
            'G' -> 'C'
            'C' -> 'G'
            'T' -> 'A'
            'A' -> 'U'
            else -> throw IllegalArgumentException("Invalid nucleotide: $it")
        }
    }.joinToString("")


/* community solution:
 *
 * https://exercism.io/tracks/kotlin/exercises/rna-transcription/solutions/9bbf4da8cf654dcbb31e01c407b2972e
 *
    val dnaToRna = mapOf('C' to 'G', 'G' to 'C', 'T' to 'A', 'A' to 'U')
    fun ofDna(input: String): String = input.map { dnaToRna[it] }.joinToString(separator = "")
 *
 */
