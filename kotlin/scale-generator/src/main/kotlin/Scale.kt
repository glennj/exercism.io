class Scale(private val tonic: String) {

    private val sharps = listOf("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
    private val flats  = listOf("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab")

    private val useFlats = listOf("F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb")

    private val interval = mapOf('m' to 1, 'M' to 2, 'A' to 3)

    fun chromatic(): List<String> = interval("m".repeat(12))

    fun interval(intervals: String): List<String> =
         with (scale()) {
            var idx = 0
            intervals.map {
                val note = this[idx]
                idx += interval[it] ?: error("Invalid interval: $it")
                note
            }
        }

    private fun scale(): List<String> {
        val notes = if (useFlats.contains(tonic)) flats else sharps
        val idx = notes.indexOf(tonic.capitalize())
        if (idx == -1) error("Invalid tonic: $tonic")
        return notes.drop(idx) + notes.take(idx)
    }
}
