module Pangram

open System

let Alphabet = seq {'a'..'z'}

let isPangram (input: string): bool = 
    let uniqLetters = input 
                      |> Seq.filter Char.IsAsciiLetter
                      |> Seq.distinctBy Char.ToLower

    Seq.length Alphabet = Seq.length uniqLetters


(* After perusing community solutions, Sets are better for this:

    set {'a'..'z'} - set (input.ToLower()) |> Set.isEmpty
*)
