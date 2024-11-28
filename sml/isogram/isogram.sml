fun isIsogram s =
  let fun letterIndex ltr = (Char.ord o Char.toUpper) ltr - Char.ord #"A"
      val presence = Array.array (26, false)
      fun process [] = true
        | process (c::cs) =
            if not (Char.isAlpha c)
            then process cs
            else let val i = letterIndex c
                 in  if Array.sub (presence, i)
                     then false
                     else let val () = Array.update (presence, i, true)
                          in  process cs
                          end
                 end
  in  process (explode s)
  end
