use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: to-rna end

include string-dict
import lists as L

DNA-RNA = [string-dict: 'G', 'C', 'C', 'G', 'T', 'A', 'A', 'U']

fun to-rna(dna):
  dna
  ^ string-explode(_)
  ^ map({(nucleotide): DNA-RNA.get-value(nucleotide)}, _)
  ^ L.join-str(_, '')
end
