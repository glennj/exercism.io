use "int-classification.sml";

fun collatz (n: int): int option =
  let
    val rec collatz' = fn(n: int, steps: int) =>
      case (classify n)
        of ODD 1  => SOME steps
         | ODD _  => collatz' (3 * n + 1, steps + 1)
         | EVEN _ => collatz' (n div 2, steps + 1)
  in
    if n < 1
    then NONE
    else collatz' (n, 0)
  end
