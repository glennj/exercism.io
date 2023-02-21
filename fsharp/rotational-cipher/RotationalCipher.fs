module RotationalCipher

open System
    
let rotate shiftKey text = 
    let rot character baseChar =
        char ((int (character - baseChar) + shiftKey) % 26) + baseChar

    let rotateCharacter (character: char): char =
        match character with
        | lower when Char.IsAsciiLetterLower lower -> rot lower 'a' 
        | upper when Char.IsAsciiLetterUpper upper -> rot upper 'A' 
        | other -> other

    String.map rotateCharacter text
