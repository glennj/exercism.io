local
  fun eq a b = a = b
  
  fun listMapUntil (predicate: ''b -> bool) (transform: ''a -> ''b) (l: ''a list): ''b list =
    case l
      of [] => []
       | x::xs => let val t = transform x
                  in  if predicate t
                      then []
                      else t :: (listMapUntil predicate transform xs)
                  end

  fun stringSubstrings (len: int) (s: string): string list =
    if s = "" then []
    else if size s < len then [s]
    else (String.substring (s, 0, len)) :: (stringSubstrings len (String.extract (s, len, NONE)))

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
    listMapUntil (eq "STOP") codon2protein (stringSubstrings 3 strand)
end