fun toRna (dna: string): string option =
  let fun nucleotide c =
        case c
          of #"C" => "G"
           | #"G" => "C"
           | #"T" => "A"
           | #"A" => "U"
           | _    => raise Domain

  in  SOME (String.translate nucleotide dna)
      handle Domain => NONE
  end
  