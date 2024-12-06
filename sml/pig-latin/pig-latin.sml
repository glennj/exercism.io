local
  val isVowel = Char.contains "aeiou"
  fun pigify' prefix [] = prefix ^ "ay"
    | pigify' "" (cs as (#"x" :: #"r" :: _)) = (implode cs) ^ "ay"
    | pigify' "" (cs as (#"y" :: #"t" :: _)) = (implode cs) ^ "ay"
    | pigify' prefix (#"q" :: #"u" :: cs)    = (implode cs) ^ prefix ^ "quay"
    | pigify' prefix (c::cs) =
        if isVowel c orelse (c = #"y" andalso prefix <> "")
        then (implode (c::cs)) ^ prefix ^ "ay"
        else pigify' (prefix ^ str c) cs
  val pigify = (pigify' "") o explode

in
  fun translate (input: string): string =
    ((String.concatWith " ") o (List.map pigify) o (String.tokens Char.isSpace)) input
end
