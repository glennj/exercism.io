use "math-utils.sml";   (* choose *)

fun rows (n: int): int list list =
  List.tabulate (n, fn i => (List.tabulate (i + 1, fn j => i choose j)))
