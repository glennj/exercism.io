proc codon2protein(codon: string): string =
  case codon
  of "AUG":                      "Methionine"
  of "UUU", "UUC":               "Phenylalanine"
  of "UUA", "UUG":               "Leucine"
  of "UCU", "UCC", "UCA", "UCG": "Serine"
  of "UAU", "UAC":               "Tyrosine"
  of "UGU", "UGC":               "Cysteine"
  of "UGG":                      "Tryptophan"
  of "UAA", "UAG", "UGA":        "STOP"
  else:
    raise newException(ValueError, "unknown codon")

proc translate*(rna: string): seq[string] =
  for i in countup(0, rna.len - 1, 3):
    var protein = codon2protein rna[i ..< i+3]
    if protein == "STOP": break
    result.add protein
