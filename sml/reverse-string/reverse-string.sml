(* functional composition *)
val reverse = (List.foldl op^ "") o (List.map str) o explode

(* recursively reversing a list of chars *) (*
fun reverse (s: string): string =
  let fun rev (reversed) ([])    = reversed
        | rev (reversed) (c::cs) = rev (c :: reversed) cs
  in  (implode o (rev []) o explode) s
  end
*)

(* swapping elements in an array *) (*
fun reverse (s: string): string =
  let val cs = Array.fromList (List.map str (explode s))
      fun rev (i, j) =
        if i >= j then ()
        else let val (a, b) = (Array.sub (cs, i), Array.sub (cs, j))
                 val (_, _) = (Array.update (cs, i, b), Array.update (cs, j, a))
             in  rev (i + 1, j - 1)
             end
      val () = rev (0, Array.length cs - 1)
  in  Array.foldr op^ "" cs
  end
*)