module PigLatin

open System.Text.RegularExpressions

(* each of these patterns have 2 sets of capturing parentheses 
 * to make processing in `translateWord` simpler 
 *)
let private patterns = 
    List.map (fun re -> Regex (re, RegexOptions.IgnoreCase)) [
        @"^()((?:[aeiou]|xr|yt).+)";   // starts with a "vowel sound"
        @"^([^aeiou]*qu)(.+)";         // starts with a Q
        @"^([^aeiou]+)(y.*)";          // starts with a Y
        @"^([^aeiou]+)(.+)" ]          // starts with a consonant

let private translateWord (m: Match): string = 
    let word = m.Groups[0].Value
    let (regex: Regex option) = List.tryFind (fun re -> re.IsMatch word) patterns 
    match regex with
    | None -> word
    | Some re ->
        let m = re.Match word
        $"{m.Groups[2].Value}{m.Groups[1].Value}ay"

let translate (input: string): string = 
    Regex.Replace (input, @"\w+", translateWord)
