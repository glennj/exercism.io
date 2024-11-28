fun distance (strand1: string, strand2: string): int option =
  let val s1      = String.explode strand1
      val s2      = String.explode strand2
      val counter = fn (a, b, c) => if a = b then c else c + 1
  in  SOME (ListPair.foldlEq counter 0 (s1, s2))
  end
  handle UnequalLengths => NONE
       