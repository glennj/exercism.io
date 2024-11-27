(* a little DSL amusement *)
infix between
fun (n between (a, z)) = a <= n andalso n <= z

val SIZE = 64

fun square (n: int): string =
  if n between (1, SIZE)
  then IntInf.toString (IntInf.pow(2, n - 1))
  else raise Fail ("square must be between 1 and " ^ (Int.toString SIZE))

fun total (): string =
  IntInf.toString (IntInf.pow(2, SIZE) - 1)
