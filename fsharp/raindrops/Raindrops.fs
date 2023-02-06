module Raindrops

let convert (number: int): string = 
    let drop divisor sound = 
        match number % divisor with
        | 0 -> sound
        | _ -> ""

    match (drop 3 "Pling") + (drop 5 "Plang") + (drop 7 "Plong") with
    | "" -> string number
    | raindrops -> raindrops