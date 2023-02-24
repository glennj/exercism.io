module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int = 
    let divisors = List.filter (fun d -> d > 0) numbers
    let isMultiple num = List.exists (fun d -> num % d = 0) divisors

    seq { 1 .. upperBound - 1 }
    |> Seq.filter isMultiple
    |> Seq.sum
