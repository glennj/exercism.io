class ProteinTranslation
  @proteins: (strand) ->
    proteins = []

    # I want to use `break` for the STOP case,
    # but it only breaks the switch, not the loop
    do ->
      while strand.length > 0
        [codon, strand] = [strand[...3], strand[3..]]
        switch codon
          when 'AUG'                      then proteins.push 'Methionine'
          when 'UUU', 'UUC'               then proteins.push 'Phenylalanine'
          when 'UUA', 'UUG'               then proteins.push 'Leucine'
          when 'UCU', 'UCC', 'UCA', 'UCG' then proteins.push 'Serine'
          when 'UAU', 'UAC'               then proteins.push 'Tyrosine'
          when 'UGU', 'UGC'               then proteins.push 'Cysteine'
          when 'UGG'                      then proteins.push 'Tryptophan'
          when 'UAA', 'UAG', 'UGA'        then return
          else throw new Error 'Invalid codon'

    proteins

module.exports = ProteinTranslation
