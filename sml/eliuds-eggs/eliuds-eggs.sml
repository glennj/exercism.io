(* a little syntactic sugar *)
infix >>
infix &
fun (n >> i) = IntInf.~>>(n, Word.fromInt i)
fun (n & m)  = IntInf.andb(n, m)

(* non-tail recursive *)
fun eggCount (0) = 0
  | eggCount (n) = eggCount (n >> 1) + (n & 1)
