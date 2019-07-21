class RnaTranscription {

    static final map = [
        'G':'C', 'C':'G', 'T':'A', 'A':'U'
    ].withDefault { throw new IllegalArgumentException() }

    def ofDNA(strand) {
        strand.inject("") { rna, nucleotide ->
            rna + map[nucleotide]
        }
    }
}
