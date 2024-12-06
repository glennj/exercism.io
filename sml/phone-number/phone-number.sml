fun clean (text: string): string option =
  let val clean = List.filter (not o (Char.contains "+() -.")) (explode text)
      val clean = if hd clean = #"1" andalso length clean > 10 then tl clean else clean
  in  if List.exists (not o Char.isDigit) clean then NONE
      else if length clean <> 10 then NONE
      else let val (area :: _ :: _ :: exchange :: _) = clean
           in  if Char.contains "01" area orelse Char.contains "01" exchange then NONE
               else SOME (implode clean)
           end
  end
  