include std/sequence.e

public function to_rna(sequence dna)
  return transmute(dna, "GCTA", "CGAU")
end function
