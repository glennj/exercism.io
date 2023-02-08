module Gigasecond

open System

// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/literals#named-literals 
[<Literal>]
let Gigasecond = 1e9

let add (beginDate: DateTime): DateTime = beginDate.AddSeconds(Gigasecond)
