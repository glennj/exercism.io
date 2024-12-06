local
  fun pigify word = pigify' "" (explode word)
  and pigify' (prefix: string) (chars: char list): string =
    case (prefix, chars)
      of ("",     #"x" :: #"r" :: _)  => (implode chars) ^ "ay"
       | ("",     #"y" :: #"t" :: _)  => (implode chars) ^ "ay"
       | (prefix, [])                 => prefix ^ "ay"
       | (prefix, #"q" :: #"u" :: cs) => (implode cs) ^ prefix ^ "quay"
       | (prefix, c::cs) =>
           if Char.contains "aeiou" c orelse (c = #"y" andalso prefix <> "")
           then (implode (c::cs)) ^ prefix ^ "ay"
           else pigify' (prefix ^ str c) cs

in
  fun translate (input: string): string =
    ((String.concatWith " ") o (List.map pigify) o (String.tokens Char.isSpace)) input
end
