module BinarySearch

let find (input: int[]) (value: int): int option = 
    let rec finder i j =
        if i > j then
            None
        else
            let mid = int ((i + j) / 2)
            match input[mid] with
            | item when value < item -> finder i (mid - 1)
            | item when value > item -> finder (mid + 1) j
            | _ -> Some mid

    finder 0 (input.Length - 1)
