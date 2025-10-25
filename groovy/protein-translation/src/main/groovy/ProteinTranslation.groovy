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

    static proteins(strand) {
        def proteins = []
        while (strand != "") {
            def codon = strand.take(3)
            def protein = codon2protein[codon]

            if (protein == null) throw new Exception("Invalid codon")
            if (protein == 'STOP') break

            proteins.add(protein)
            strand = strand.drop(3)
        }
        return proteins
    }
}
