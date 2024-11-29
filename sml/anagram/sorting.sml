(* insertion sort ideas stolen from
 * https://exercism.org/tracks/sml/exercises/isogram/solutions/nzok
 *)
fun sort p lst =
  let fun insertSorted (item, []) = [item]
        | insertSorted (item, head::tail) =
            if p(item, head)
            then item::head::tail
            else head::(insertSorted (item, tail))
  in  List.foldl insertSorted [] lst
  end
