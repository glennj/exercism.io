local
  fun double (digit, isDouble) =
    if isDouble then List.nth ([0,2,4,6,8,1,3,5,7,9], digit)
    else             List.nth ([0,1,2,3,4,5,6,7,8,9], digit)

  fun luhnSum ([], _) = 0
    | luhnSum (d::ds, isDouble) = double (d, isDouble) + luhnSum (ds, not isDouble)
in
  fun valid (value: string): bool =
    let 
      val cleaned  = String.concat (String.tokens Char.isSpace value)
      val digits   = map (Int.fromString o str) (explode cleaned)
      val isDouble = (length digits) mod 2 = 0
    in
      if length digits <= 1 then false
      else luhnSum (map valOf digits, isDouble) mod 10 = 0
      handle Option => false (* non-digit character *)
    end
end
