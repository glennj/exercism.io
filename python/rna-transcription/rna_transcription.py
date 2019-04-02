'''
# take 1
def to_rna(dna_strand):
    return "".join(DNA2RNA[c] for c in dna_strand)
'''

'''
# take 2
DNA2RNA = {'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'}
DNA = "".join(DNA2RNA.keys())

def to_rna(dna_strand):
    rna = ""
    for c in dna_strand:
        if c not in DNA:
            raise ValueError('Invalid DNA strand')
        rna += DNA2RNA[c]
    return rna

'''

DNA2RNA = str.maketrans('GCTA', 'CGAU')

def to_rna(dna_strand):
    return dna_strand.translate(DNA2RNA)

