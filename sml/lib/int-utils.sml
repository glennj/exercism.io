fun intPow(base: int, exp: int): int =
  let
    fun pow'(0, result)   = result
      | pow'(exp, result) = pow'(exp - 1, base * result)
  in
    if exp < 0
    then raise Domain
    else pow'(exp, 1)
  end

infix **
fun (base ** exp) = intPow(base, exp)

fun intWidth(n: int): int =
  if n < 0
  then raise Domain
  else if n = 0
       then 1
       else 1 + floor(Math.log10(real n))
