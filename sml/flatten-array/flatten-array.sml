val rev = List.rev

(* Rose tree *)
datatype 'a tree = Empty | Elem of 'a | List of 'a tree list

fun flatten (tree: 'a tree): 'a list =
  let
    fun f' acc (Empty)         = acc
      | f' acc (Elem e)        = e :: acc
      | f' acc (List [])       = acc
      | f' acc (List (h :: t)) = f' (f' acc h) (List t)
  in
    rev (f' [] tree)
  end
