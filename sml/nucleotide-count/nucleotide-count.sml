fun nucleotideCounts (strand: string): {a: int, c: int, g: int, t: int} =
  let fun counter (nucleotide, (a, c, g, t)) = 
        case nucleotide
          of #"A" => (a + 1, c, g, t)
           | #"C" => (a, c + 1, g, t)
           | #"G" => (a, c, g + 1, t)
           | #"T" => (a, c, g, t + 1)
           | _    => raise Fail "Invalid nucleotide in strand"
      val (a, c, g, t) = List.foldl counter (0, 0, 0, 0) (explode strand)
  in  {a = a, c = c, g = g, t = t}
  end

(*
local
  fun count [] result = result
    | count (#"A" :: rest) (a, c, g, t) = count rest (a + 1, c, g, t)
    | count (#"C" :: rest) (a, c, g, t) = count rest (a, c + 1, g, t)
    | count (#"G" :: rest) (a, c, g, t) = count rest (a, c, g + 1, t)
    | count (#"T" :: rest) (a, c, g, t) = count rest (a, c, g, t + 1)
    | count _ _ = raise Fail "Invalid nucleotide in strand"
in
  fun nucleotideCounts (strand: string): {a: int, c: int, g: int, t: int} =
    let val (a, c, g, t) = count (explode strand) (0, 0, 0, 0)
    in  {a = a, c = c, g = g, t = t}
    end
end
*)
