module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec helper acc items =
        match items with
        | [] -> acc |> List.rev
        | item :: rest -> helper (func item :: acc) rest

    helper [] input
        

