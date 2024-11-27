datatype 'a classification = EVEN of 'a
                           | ODD of 'a

fun classify (n: int): int classification =
  (* Q: how to use modules in basis library? 
   * if IntInf.andb(n, 1) = 0
   *)
  if n mod 2 = 0
  then EVEN n
  else ODD n
