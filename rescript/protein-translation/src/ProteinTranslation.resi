type aminoAcids =
  | Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  | STOP

let proteins: string => option<array<aminoAcids>>
