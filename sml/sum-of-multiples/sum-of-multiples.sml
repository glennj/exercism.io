local
  fun range (a: int) (b: int): int list =
    List.tabulate (b - a + 1, fn i => a + i)
  
  fun hasFactor (factors: int list) (n: int): bool =
    List.exists (fn f => n mod f = 0) factors
  
  val addUp = List.foldl op+ 0
in
  fun sum (factors: int list, limit: int): int =
    if null factors
    then 0
    else addUp (List.filter (hasFactor factors) (range 1 (limit - 1)))
end
