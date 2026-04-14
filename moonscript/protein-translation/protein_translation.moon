STOP = 'STOP'

to_protein = (codon) ->
  switch codon
    when 'AUG'                      then 'Methionine'
    when 'UUU', 'UUC'               then 'Phenylalanine'
    when 'UUA', 'UUG'               then 'Leucine'
    when 'UCU', 'UCC', 'UCA', 'UCG' then 'Serine'
    when 'UAU', 'UAC'               then 'Tyrosine'
    when 'UGU', 'UGC'               then 'Cysteine'
    when 'UGG'                      then 'Tryptophan'
    when 'UAA', 'UAG', 'UGA'        then STOP
    else error 'Invalid codon'
    
{
  proteins: (strand) ->
    ps = {}
    while strand != ""
      codon, strand = strand\match '^(...)(.*)'
      protein = to_protein codon
      break if protein == STOP
      table.insert ps, protein
    ps
}
