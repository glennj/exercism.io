module BeerSong

(* Taking (excessive) care to not repeat any strings. *)

let private bottles num =
    let n = if num = 0 then "No more" else string num
    let s = if num = 1 then "" else "s"
    $"{n} bottle{s} of beer"

let private where = "on the wall"

let private firstLine (n: int): string =
    let b = bottles n
    $"{b} {where}, {b.ToLower()}."

let private secondLine (n: int): string =
    let one num = if num = 1 then "it" else "one"
    let b, action =
        match n with
        | 0 -> ((bottles 99), "Go to the store and buy some more")
        | _ -> ((bottles (n-1)), $"Take {one n} down and pass it around")
    $"{action}, {b.ToLower()} {where}." 

let recite (startBottles: int) (takeDown: int) = 
    [startBottles .. -1 .. (startBottles - takeDown + 1)]
    |> List.map (fun i -> [""; (firstLine i); (secondLine i)])
    |> List.concat
    |> List.skip 1