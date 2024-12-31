val colors = [ "black", "brown", "red", "orange", "yellow"
             , "green", "blue", "violet", "grey", "white"
             ]

fun colorCode color =
  let fun code ([], _)    = raise Domain
        | code (c::cs, i) = if c = color then i else code (cs, i + 1)
  in  code (colors, 0)
  end
