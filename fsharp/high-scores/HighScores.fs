module HighScores

//let scores (values: int list): int list = values
let scores = id

//let latest (values: int list): int = List.last values
let latest = List.last

//let personalBest (values: int list): int = failwith "You need to implement this function."
let personalBest = List.max

(*
let personalTopThree (values: int list): int list =
    let rec myTake (howMany: int) (taken: int list) (values: int list): int list =
        if howMany = 0 || values.Length = 0 then 
            List.rev taken
        else
            myTake (howMany - 1) ((List.head values) :: taken) (List.tail values)

    values |> List.sortDescending |> myTake 3 []
*)
// thanks to https://exercism.org/tracks/fsharp/exercises/high-scores/solutions/shawn-martin
let personalTopThree: (int list -> int list) = List.sortDescending >> (List.truncate 3)
