DNA2RNA = {'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'}
DNA = "".join(DNA2RNA.keys())

def to_rna(dna_strand):
    '''
    return "".join(DNA2RNA[c] for c in dna_strand)
    '''
    rna = ""
    for c in dna_strand:
        if c not in DNA:
            raise ValueError('Invalid DNA strand')
        rna += DNA2RNA[c]
    return rna
