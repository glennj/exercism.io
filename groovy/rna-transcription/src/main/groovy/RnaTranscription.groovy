class RnaTranscription {

    static final map = [
        'G':'C', 'C':'G', 'T':'A', 'A':'U'
    ].withDefault { throw new IllegalArgumentException() }

    static toRna(dna) {
        dna.inject("") { rna, nucleotide ->
            rna + map[nucleotide]
        }
    }
}
