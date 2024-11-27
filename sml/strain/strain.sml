fun keep f l =
  let val rec keep' = fn ([], kept)    => kept
                       | (x::xs, kept) => if f x
                                          then keep'(xs, x :: kept)
                                          else keep'(xs, kept)
  in  (rev o keep') (l, [])
  end

fun discard f l = keep (fn x => (not o f) x) l
