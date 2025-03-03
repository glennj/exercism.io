fun convert n =
  let val addDrop = fn(m, drop, acc) => acc ^ (if n mod m = 0 then drop else "")
      val sounds  = ListPair.foldl addDrop "" ([3, 5, 7], ["Pling", "Plang", "Plong"])
  in  if (size sounds) > 0
      then sounds
      else Int.toString n
  end

