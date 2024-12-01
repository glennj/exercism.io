datatype classification = Abundant | Deficient | Perfect

local
  fun factors n =
    let fun f' f fs =
      if f * f > n
      then fs
      else if n mod f <> 0
           then f' (f + 1) fs
           else let val g = n div f
                in if f = g
                   then f' (f + 1) (f :: fs)
                   else f' (f + 1) (f :: g :: fs)
                end
    in  f' 1 []
    end

  fun aliquotSum n = (List.foldl op+ 0 (factors n)) - n
in
  fun classify (input: int): classification option =
    if input <= 0
    then NONE
    else SOME (case Int.compare (aliquotSum input, input)
                of EQUAL   => Perfect
                 | GREATER => Abundant
                 | LESS    => Deficient
              )
end
