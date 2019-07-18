class ProteinTranslation {

    private static codon2protein = [
        'AUG': 'Methionine',
        'UUU': 'Phenylalanine',
        'UUC': 'Phenylalanine',
        'UUA': 'Leucine',
        'UUG': 'Leucine',
        'UCU': 'Serine',
        'UCC': 'Serine',
        'UCA': 'Serine',
        'UCG': 'Serine',
        'UAU': 'Tyrosine',
        'UAC': 'Tyrosine',
        'UGU': 'Cysteine',
        'UGC': 'Cysteine',
        'UGG': 'Tryptophan',
        'UAA': 'STOP',
        'UAG': 'STOP',
        'UGA': 'STOP',
    ]

    /* first take
    static proteins(strand) {
        def proteins = []
        def codons = strand.findAll("...")
        for (codon in codons) {
            def protein = codon2protein[codon] ?: 'STOP'
            if (protein == 'STOP') break
            proteins << protein
        }
        proteins
    }
    */

    static proteins(strand) {
        strand.findAll("...")
              .collect { codon2protein[it] ?: 'STOP' }
              .takeWhile { it != 'STOP' }
    }
}
