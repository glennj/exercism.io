use "list-utils.sml";   (* slice, seq *)

datatype relation =
    Equal
  | Superlist
  | Sublist
  | Unequal

local
  (* because `list1 = list2` is just too simple *)
  fun equals (l1: ''a list, l2: ''a list): bool =
    ListPair.allEq op= (l1, l2)
  infix equals

  fun containsList (haystack: ''a list, needle: ''a list): bool =
    let val len = length needle
    in  List.exists (fn i => needle equals (slice (haystack, i, len)))
                    (seq (length haystack - len + 1))
    end
  infix containsList

in
  fun sublist (listOne: int list, listTwo: int list): relation =
    if listOne equals listTwo then Equal
    else if length listOne < length listTwo andalso listTwo containsList listOne then Sublist
    else if listOne containsList listTwo then Superlist
    else Unequal
end
