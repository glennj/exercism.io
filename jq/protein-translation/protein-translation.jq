include "lib/assert";

def trim_stop:
  (index("STOP") // length) as $stop_idx
  | .[0:$stop_idx]
;

{
  "AUG": "Methionine",
  "UUU": "Phenylalanine",
  "UUC": "Phenylalanine",
  "UUA": "Leucine",
  "UUG": "Leucine",
  "UCU": "Serine",
  "UCC": "Serine",
  "UCA": "Serine",
  "UCG": "Serine",
  "UAU": "Tyrosine",
  "UAC": "Tyrosine",
  "UGU": "Cysteine",
  "UGC": "Cysteine",
  "UGG": "Tryptophan",
  "UAA": "STOP",
  "UAG": "STOP",
  "UGA": "STOP"
} as $proteins

| [.strand | scan(".{1,3}")]
| map($proteins[.])
| trim_stop
| assert(all; "Invalid codon")
