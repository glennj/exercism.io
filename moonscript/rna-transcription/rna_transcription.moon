complement = A:'U', C:'G', G:'C', T:'A'

return (dna) -> dna\gsub "[ACGT]", (nuc) -> complement[nuc]
