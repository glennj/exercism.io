local
  val SMALLS = [ (0, "zero"), (1, "one"), (2, "two"), (3, "three"), (4, "four")
               , (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")
               , (10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen")
               , (14, "fourteen"), (15, "fifteen"), (16, "sixteen")
               , (17, "seventeen"), (18, "eightteen"), (19, "nineteen")
               ]

  val TENS = [ (20, "twenty"), (30, "thirty"), (40, "forty"), (50, "fifty")
             , (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety")
             ]
in
  fun say (number: int): string =
    if number < 0 orelse number > 999999999999 then raise Fail "input out of range"
    else if number < 100        then saySmall number
    else if number < 1000       then sayCompound number 100 "hundred"
    else if number < 1000000    then sayCompound number 1000 "thousand"
    else if number < 1000000000 then sayCompound number 1000000 "million"
    else                             sayCompound number 1000000000 "billion"

  and saySmall (num: int): string =
    case List.find (fn pair => num = #1 pair) SMALLS
      of SOME(pair) => #2 pair
       | NONE =>
           case List.find (fn pair => num = #1 pair) TENS
             of SOME(pair) => #2 pair
              | NONE =>
                  let val r = num mod 10
                  in  say (num - r) ^ "-" ^ say r
                  end

  and sayCompound num base word =
    let val (n, r) = (num div base, num mod base)
    in  say n ^ " " ^ word ^ (if r = 0 then "" else " " ^ say r)
    end
end
