import tables, sequtils

proc countDna*(strand: string): CountTable[char] =
  if anyIt(strand, it notin "ACGT"):
    raise newException(ValueError, "invalid nucleotide")
  strand.toCountTable
