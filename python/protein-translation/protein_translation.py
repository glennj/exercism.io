# map protein to list of codons
P2C = {
    'Methionine': ['AUG'],
    'Phenylalanine': ['UUU', 'UUC'],
    'Leucine': ['UUA', 'UUG'],
    'Serine': ['UCU', 'UCC', 'UCA', 'UCG'],
    'Tyrosine': ['UAU', 'UAC'],
    'Cysteine': ['UGU', 'UGC'],
    'Tryptophan': ['UGG'],
    'STOP': ['UAA', 'UAG', 'UGA'],
}

# map codon to protein
C2P = {c: p for p in P2C for c in P2C[p]}


def proteins(strand):
    proteins = []
    for i in range(0, len(strand), 3):
        codon = strand[i:i+3]
        protein = C2P[codon]
        if protein == 'STOP':
            break
        proteins.append(protein)
    return proteins
