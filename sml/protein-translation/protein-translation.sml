use "string-utils.sml";         (* stringSubstrings *)
use "list-utils.sml";           (* mapUntil *)

local
  fun eq a b = a = b
  
  fun codon2protein codon =
    case codon
      of "AUG" => "Methionine"
       | "UUC" => "Phenylalanine" | "UUU" => "Phenylalanine"
       | "UCA" => "Serine" | "UCC" => "Serine" | "UCG" => "Serine" | "UCU" => "Serine"
       | "UUA" => "Leucine" | "UUG" => "Leucine"
       | "UAC" => "Tyrosine" | "UAU" => "Tyrosine"
       | "UGG" => "Tryptophan"
       | "UGC" => "Cysteine" | "UGU" => "Cysteine"
       | "UAA" => "STOP" | "UAG" => "STOP" | "UGA" => "STOP"
       | _     => raise Fail "Invalid codon"
in
  fun proteins (strand: string): string list = 
    mapUntil (eq "STOP") codon2protein (stringSubstrings 3 strand)
end
