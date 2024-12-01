fun abbreviate (phrase: string): string =
  let
    fun abbr _ [] inits = (implode o (List.map Char.toUpper) o List.rev) inits
      | abbr true (c::cs) inits = (* seeking a the start of a word *)
          if Char.isAlpha c
          then abbr false cs (c :: inits)
          else abbr true  cs inits
      | abbr false (c::cs) inits = (* seeking the end of a word *)
          if Char.isAlpha c orelse c = #"'"
          then abbr false cs inits
          else abbr true  cs inits
  in
    abbr true (explode phrase) []
  end
