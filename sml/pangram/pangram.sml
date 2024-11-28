fun isPangram s =
  let fun letterIndex ltr = (Char.ord o Char.toUpper) ltr - Char.ord #"A"
      val presence = Array.array(26, false)
      fun seen c = Array.update (presence, letterIndex c, true)
      val () = List.app (fn c => if Char.isAlpha c then seen c else ()) (explode s)
  in  Array.all (fn(x) => x) presence
  end
  