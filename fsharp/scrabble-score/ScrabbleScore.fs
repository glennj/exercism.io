module ScrabbleScore

let score (word: string): int = 
    let letterScore =
        function
        | 'A' | 'E' | 'I' | 'O' | 'U' -> 1
        | 'L' | 'N' | 'R' | 'S' | 'T' -> 1
        | 'D' | 'G' -> 2
        | 'B' | 'C' | 'M' | 'P' -> 3
        | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
        | 'K' -> 5
        | 'J' | 'X' -> 8
        | 'Q' | 'Z' -> 10
        | _ -> 0

    word.ToUpper() 
    |> Seq.sumBy letterScore
