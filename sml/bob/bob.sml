fun response s =
  let val cs      = List.filter (not o Char.isSpace) (String.explode s)
      val silent  = null cs
      val asking  = not silent andalso List.last cs = #"?"
      val yelling = not silent
                    andalso List.exists Char.isAlpha cs
                    andalso List.all (not o Char.isLower) cs

  in  if silent                      then "Fine. Be that way!"
      else if yelling andalso asking then "Calm down, I know what I'm doing!"
      else if yelling                then "Whoa, chill out!"
      else if asking                 then "Sure."
      else                                "Whatever."
  end
