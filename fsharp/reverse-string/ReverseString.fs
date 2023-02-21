module ReverseString

let reverse (input: string): string = 
    let rec reverser chars (reversed: char list) =
        match chars with
        | [] ->  System.String.Concat reversed
        | c :: cs -> reverser cs (c :: reversed)
    reverser (Seq.toList input) []
