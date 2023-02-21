module ProteinTranslation

(* third attempt, having viewed the community solutions *)
let proteins (rna: string): string list = 
    let mapping = 
        function
        | "AUG"                         -> "Methionine"
        | "UUU" | "UUC"                 -> "Phenylalanine"
        | "UUA" | "UUG"                 -> "Leucine"
        | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
        | "UAU" | "UAC"                 -> "Tyrosine"
        | "UGU" | "UGC"                 -> "Cysteine"
        | "UGG"                         -> "Tryptophan"
        | "UAA" | "UAG" | "UGA"         -> "STOP"
        | _ -> failwith "unknown codon"

    rna
    |> Seq.chunkBySize 3
    |> Seq.map (System.String >> mapping)
    |> Seq.takeWhile (fun protein -> protein <> "STOP")
    |> Seq.toList


(* second attempt
let proteins (rna: string): string list = 
    let mapping = 
        function
        | "AUG"                         -> "Methionine"
        | "UUU" | "UUC"                 -> "Phenylalanine"
        | "UUA" | "UUG"                 -> "Leucine"
        | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
        | "UAU" | "UAC"                 -> "Tyrosine"
        | "UGU" | "UGC"                 -> "Cysteine"
        | "UGG"                         -> "Tryptophan"
        | "UAA" | "UAG" | "UGA"         -> "STOP"
        | _ -> failwith "unknown codon"

    let rec codons2proteins cs ps =
        match String.length cs with
        | len when len < 3 -> List.rev ps
        | _ ->
            match mapping cs[0..2] with
            | "STOP" -> List.rev ps
            | p -> codons2proteins (cs.Substring 3) (p :: ps)

    codons2proteins rna []
*)

(* first attempt
let proteins (rna: string): string list = 
    let rec helper rna proteins =
        if String.length rna < 3 then
            List.rev proteins
        else
            match rna[0..2] with
            | "UAA" | "UAG" | "UGA" -> List.rev proteins  // STOP
            | "AUG"                         -> helper (rna.Substring 3) ("Methionine" :: proteins)
            | "UUU" | "UUC"                 -> helper (rna.Substring 3) ("Phenylalanine" :: proteins)
            | "UUA" | "UUG"                 -> helper (rna.Substring 3) ("Leucine" :: proteins)
            | "UCU" | "UCC" | "UCA" | "UCG" -> helper (rna.Substring 3) ("Serine" :: proteins)
            | "UAU" | "UAC"                 -> helper (rna.Substring 3) ("Tyrosine" :: proteins)
            | "UGU" | "UGC"                 -> helper (rna.Substring 3) ("Cysteine" :: proteins)
            | "UGG"                         -> helper (rna.Substring 3) ("Tryptophan" :: proteins)
            | _                             -> helper (rna.Substring 3) proteins

    helper rna []
*)
