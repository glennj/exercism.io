module RunLengthEncoding

open System.Text.RegularExpressions

let decode (input: string): string =
    let re = Regex @"(\d+)(\D)"
    let decoder (m: Match) =
        let n, c = (m.Groups[1].Value, m.Groups[2].Value)
        String.replicate (int n) c
    re.Replace (input, decoder)

let encode (input: string): string =
    let re = Regex @"(.)\1+"
    let encoder (m: Match) =
        $"{String.length m.Value}{m.Groups[1].Value}"
    re.Replace (input, encoder)
