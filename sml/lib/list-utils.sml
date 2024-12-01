(* extract the unique elements of a list *)
fun 'a uniqBy (eq: 'a * 'a -> bool) (xs: 'a list): 'a list = 
  let fun uniq' _ [] us = us
        | uniq' eq (x::xs) us =
            uniq' eq xs (if List.exists (fn u => eq(u, x)) us then us else x::us)
  in  uniq' eq xs []
  end

fun uniqReal (xs: real list): real list = uniqBy Real.== xs

(* for equality types (int, char, string, bool) *)
fun uniq (xs: ''a list): ''a list = uniqBy op= xs
