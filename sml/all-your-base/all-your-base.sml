fun toDecimal(base, [], dec)    = SOME dec
  | toDecimal(base, d::ds, dec) =
      if d < 0 orelse d >= base
      then NONE
      else toDecimal(base, ds, dec * base + d)
      
fun toOutBase(b, 0, ds) = ds
  | toOutBase(b, n, ds) = toOutBase(b, n div b, (n mod b) :: ds)

fun rebase (inBase: int, outBase: int, digits: int list): int list option =
  if inBase < 2 orelse outBase < 2
  then NONE
  else case toDecimal(inBase, digits, 0)
         of NONE   => NONE
          | SOME 0 => NONE
          | SOME d => SOME (toOutBase(outBase, d, []))
