use "list-utils.sml";   (* seq *)
use "string-utils.sml"; (* pad, trim *)

local
  (* F#-style pipe operator: value on left-hand side becomes
   * the right-hand side's _last_ argument *)
  infix |>
  fun (x |> f) = f x

  fun transpose' l =
    let val heightSeq = seq (length l)
        val widthSeq  = seq (size (hd l))
    in  List.map
          (fn i => List.map (fn j => substring (List.nth (l, j), i, 1)) heightSeq
                   |> String.concat)
          widthSeq
    end

  (* in a list of strings, each string is _at least_ as long as the following one. *)
  fun padUp l =
    let fun pad' (s, (wid, acc)) =
          let val newWid = Int.max (wid, size s)
          in  (newWid, (pad newWid " " s) :: acc)
          end 
    in  List.foldr pad' (~1, []) l |> #2
    end

in
  fun transpose (lines: string list): string list =
    let val maxWidth = List.map size lines |> List.foldl Int.max ~1 
    in  lines
        |> List.map (pad maxWidth " ")
        |> transpose'
        |> List.map (trim " ")
        |> padUp
    end
    handle Empty => []
end
