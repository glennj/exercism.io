module Isogram

open System

let private sanitize (str: string) =
    str.ToUpper ()
    |> Seq.filter Char.IsLetter
    |> Seq.toList

let isIsogram (str: string): bool = 
    let input = sanitize str

    (* simple solution:
    input.Length = (set input).Count
    *)

    (* more fun solution *)
    let rec helper chars seen =
        match chars with
        | [] -> true
        | c :: _ when Set.contains c seen -> false
        | c :: cs -> helper cs (Set.add c seen)

    helper input Set.empty
