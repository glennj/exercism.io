module Seq

// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#sequence-expressions
let keep pred xs = [ for x in xs do if pred x then yield x ]

(*
let discard pred xs = keep (fun x -> not (pred x)) xs
*)
(*
// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#function-composition
let discard pred xs = keep (pred >> not) xs
*)

// https://fsharpforfunandprofit.com/posts/convenience-partial-application/
// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#partial-application-of-arguments
let discard pred = keep (pred >> not)
