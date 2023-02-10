module PerfectNumbers

open System

type Classification = Perfect | Abundant | Deficient 

let private factors n: int list =
    [ for i = 1 to (int (sqrt (float n))) do
        let f, rem = Math.DivRem(n, i)
        if rem = 0 then yield! List.distinct [i; f] ]
        
let private aliquotSum n = List.sum (factors n) - n

let classify (n: int) : Classification option = 
    if n < 1 then
        None
    else
        Some ( match (aliquotSum n) with
               | a when a = n -> Classification.Perfect
               | a when a < n -> Classification.Deficient
               | _ -> Classification.Abundant )