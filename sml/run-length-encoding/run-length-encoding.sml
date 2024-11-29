fun encode (phrase: string): string =
  let 
    fun run (0, _) = ""
      | run (1, c) = str c
      | run (n, c) = (Int.toString n) ^ str c

    fun encode' ([],    prev, count, acc) = acc ^ run (count, prev)
      | encode' (c::cs, prev, count, acc) =
          if prev = c
          then encode' (cs, prev, count + 1, acc)
          else encode' (cs, c, 1, acc ^ run (count, prev))
  in
    encode' (explode phrase, chr 0, 0, "")
  end

fun decode (phrase: string): string =
  let
    fun strRepeat (s, 0) = s  (* odd special case *)
      | strRepeat (s, 1) = s
      | strRepeat (s, n) = s ^ strRepeat (s, n-1)

    fun decode' ([], _, acc)        = acc
      | decode' (c::cs, count, acc) =
          if Char.isDigit c
          then decode' (cs, 10 * count + Char.ord c - Char.ord #"0", acc)
          else decode' (cs, 0, acc ^ strRepeat (str c, count))
  in
    decode' (explode phrase, 0, "")
  end
