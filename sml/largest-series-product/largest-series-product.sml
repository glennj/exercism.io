local
  fun digits (s: string): int list =
    List.map (valOf o Int.fromString o String.str) (explode s)
    handle Option => raise Fail "digits input must only contain digits"

  fun slice (l: 'a list, offset: int, len: int): 'a list =
    List.take (List.drop (l, offset), len)
    
  fun spans (span: int) (l: 'a list): 'a list list =
    if span < 0 then raise Fail "span must not be negative"
    else if span > length l then raise Fail "span must be smaller than string length"
    else List.foldr
           (fn (i, acc) => (slice (l, i, span)) :: acc)
           []
           (List.tabulate (length l - span + 1, fn i => i))
  
  fun largest [] = 1 (* special case for this exercise *)
    | largest ns = List.foldl Int.max 0 ns

  val product = List.foldl op* 1
  
  infix |>
  fun (x |> f) = f x
in
  fun largestProduct (digitStr: string, span: int): int =
    digitStr
    |> digits
    |> spans span
    |> List.map product
    |> largest
end