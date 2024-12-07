use "int-utils.sml";    (* infix `**` *)
use "math-utils.sml";   (* log *)

(* Using the Binary numeral system (base 2) from Wikipedia
 * https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
 *)

fun squareRoot (radicand: int): int =
  let fun sqrt' (_, x, 0) = x
        | sqrt' (n, x, b) =
            if n >= x + b
            then sqrt' (n - x - b, (x div 2) + b, b div 4)
            else sqrt' (n, x div 2, b div 4)
      val b = 4 ** floor (log (real radicand, 4))
  in  sqrt' (radicand, 0, b)
  end
