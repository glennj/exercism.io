use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: proteins end

fun invalid(): raise("Invalid codon") end

fun codon2protein(codon):
  ask:
    | 'AUG' == codon then: 'Methionine'
    | 'UGG' == codon then: 'Tryptophan'
    | [list: 'UUU', 'UUC'].member(codon) then: 'Phenylalanine'
    | [list: 'UUA', 'UUG'].member(codon) then: 'Leucine'
    | [list: 'UAU', 'UAC'].member(codon) then: 'Tyrosine'
    | [list: 'UGU', 'UGC'].member(codon) then: 'Cysteine'
    | [list: 'UCU', 'UCC', 'UCA', 'UCG'].member(codon) then: 'Serine'
    | [list: 'UAA', 'UAG', 'UGA'].member(codon) then: 'STOP'
    | otherwise: invalid()
  end
end

fun proteins(strand):
  rec translate = lam(s, ps):
    ask:
      | s == '' then: ps
      | string-length(s) < 3 then: invalid()
      | otherwise:
          codon = string-substring(s, 0, 3)
          protein = codon2protein(codon)
          if protein == 'STOP':
            ps
          else:
            rna = string-substring(s, 3, string-length(s))
            translate(rna, ps + [list: protein])
          end
    end
  end
  translate(strand, [list:])
end
