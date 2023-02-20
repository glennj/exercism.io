module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length <> strand2.Length 
    then 
        None
    else
        let adder dist a b = dist + if a = b then 0 else 1
        Seq.fold2 adder 0 strand1 strand2
        |> Some
