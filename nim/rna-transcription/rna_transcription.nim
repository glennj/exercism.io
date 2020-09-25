from sequtils import foldl

proc dna2rna(nucleotide: char): char =
  case nucleotide
  of 'G': 'C'
  of 'C': 'G'
  of 'T': 'A'
  of 'A': 'U'
  else: raise newException(ValueError, "invalid nucleotide")

proc toRna*(dna: string): string = 
  foldl(dna, a & dna2rna(b), "")
