module Bob

open System

let response (input: string): string = 
    let has_upper = Seq.exists Char.IsAsciiLetterUpper input
    let has_lower = Seq.exists Char.IsAsciiLetterLower input
    let trimmed = input.TrimEnd()

    let is_silence  = Seq.isEmpty trimmed
    let is_question = trimmed.EndsWith('?')
    let is_shouting = has_upper && not has_lower

    match (is_silence, is_shouting, is_question) with
    | (true, _, _)    -> "Fine. Be that way!"
    | (_, true, true) -> "Calm down, I know what I'm doing!"
    | (_, true, _)    -> "Whoa, chill out!"
    | (_, _, true)    -> "Sure."
    | _               -> "Whatever."
    
