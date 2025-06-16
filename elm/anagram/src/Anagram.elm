module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    let
        toKey = String.toList >> List.sort >> String.fromList

        target = String.toLower word
        targKey = toKey target

        isAnagram candidate =
            let lc = String.toLower candidate
            in target /= lc && targKey == toKey lc
    in
    List.filter isAnagram candidates
