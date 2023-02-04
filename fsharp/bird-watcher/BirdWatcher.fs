(* Despite the hints, I'm not assuming that every array will be of length 7 *)

module BirdWatcher

let lastWeek: int[] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday(counts: int[]): int =
    match counts with
    | [| _ |] -> failwith "No count for yesterday"
    | _ -> counts[counts.Length - 2]
    
let total(counts: int[]): int = counts |> Array.sum

let dayWithoutBirds(counts: int[]): bool = counts |> Array.contains 0

let incrementTodaysCount(counts: int[]): int[] =
    let idx = counts.Length - 1
    counts[idx] <- counts[idx] + 1
    counts

let oddWeek(counts: int[]): bool =
    let evenOrOdd remainder =
        counts
        |> Array.indexed 
        |> Array.filter (fun elem -> (fst elem) % 2 = remainder)
        |> Array.map (snd)

    // odd _days of the week_ have an array index that is *even*
    let odds = evenOrOdd 0
    let evens = evenOrOdd 1

    (evens |> Array.forall (fun elem -> elem =  0)) ||
    (evens |> Array.forall (fun elem -> elem = 10)) ||
    (odds  |> Array.forall (fun elem -> elem =  5))
