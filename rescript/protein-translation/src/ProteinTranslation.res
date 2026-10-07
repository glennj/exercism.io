type aminoAcids =
  | Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  | STOP

let mapping = dict{
  "AUG": Methionine,      "UAU": Tyrosine,
  "UUU": Phenylalanine,   "UAC": Tyrosine,
  "UUC": Phenylalanine,   "UGU": Cysteine,
  "UUA": Leucine,         "UGC": Cysteine,
  "UUG": Leucine,         "UGG": Tryptophan,
  "UCU": Serine,          "UAA": STOP,
  "UCC": Serine,          "UAG": STOP,
  "UCA": Serine,          "UGA": STOP,
  "UCG": Serine,
}

let proteins = sequence => {
  // returns a Result
  let rec translator = (seq, proteins) =>
    switch String.length(seq) {
    | 0 => Ok(proteins)
    | _ =>
      let codon = String.substring(seq, ~start=0, ~end=3)
      switch mapping->Dict.get(codon) {
      | None => Error("invalid codon")
      | Some(protein) =>
        switch protein {
        | STOP => Ok(proteins)
        | _ => {
          proteins->Array.push(protein)
          translator(String.substring(seq, ~start=3), proteins)
          }
        }
      }
    }

  switch translator(sequence, []) {
  | Error(_) => None
  | Ok(proteins) => Some(proteins)
  }
}
