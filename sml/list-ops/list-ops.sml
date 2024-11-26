fun append (list1: int list, list2: int list): int list =
  list1 @ list2

fun concat ([]: int list list): int list = []
  | concat (lst::lists) = append (lst, concat lists)
  
fun length ([]: int list) = 0
  | length (n::ns) = 1 + length(ns)

fun map (function: int -> int, []: int list): int list = []
  | map (function, n::ns) = (function n) :: map (function, ns)

fun filter (function: int -> bool, []: int list): int list = []
  | filter (function, n::ns) =
      if function n
        then n :: filter (function, ns)
        else filter (function, ns)

fun reverse ([]: int list): int list = []
  | reverse (n::ns) = append ((reverse ns), [n])

fun foldl (function: int * int -> int, initial: int, []: int list): int = initial
  | foldl (function, initial, n::ns) = foldl (function, (function (initial, n)), ns)

fun foldr (function: int * int -> int, initial: int, list: int list): int =
  foldl ((fn (x, y) => function (y, x)), initial, (reverse list))
