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

(* create a list of numbers from 0 to n-1 *)
fun seq (n: int): int list = 
  List.tabulate (n, fn i => i)

(* extract a sublist *)
fun slice (l: 'a list, offset: int, len: int): 'a list = 
  List.take (List.drop (l, offset), len)

(* kind of a mashup of `take` and `map` *)
fun mapUntil (predicate: ''b -> bool) (transform: ''a -> ''b) (l: ''a list): ''b list =
  case l
    of [] => []
     | x::xs => let val t = transform x
                in  if predicate t
                    then []
                    else t :: (mapUntil predicate transform xs)
                end
