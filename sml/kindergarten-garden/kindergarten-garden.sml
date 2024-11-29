local
  val students = Array.fromList [ "Alice", "Bob", "Charlie", "David", "Eve"
                                , "Fred", "Ginny", "Harriet", "Ileana", "Joseph"
                                , "Kincaid", "Larry"
                                ]

  fun studentIndex student =
    case Array.findi (fn (_, s) => s = student) students
      of SOME (i, _) => i
       | NONE        => raise Domain

  fun plant p =
    case p of #"C" => "clover" | #"G" => "grass" | #"R" => "radishes" | #"V" => "violets"
            | _ => raise Domain

  fun parse plots [] = plots
    | parse plots ((a, c) :: (b, d) :: rest) =
        parse ((List.map plant [a, b, c, d]) :: plots) rest
    | parse _ _ = raise Fail "malformed diagram"

in
  fun plants (diagram: string) (student: string): string list =
    let val rows  = ((List.map explode) o (String.tokens Char.isSpace)) diagram
        val rows2 = (hd rows, (hd o tl) rows)    (* 2-tuple of the first elements in `rows` *)
        val plots = (List.rev o (parse []) o ListPair.zip) rows2
    in  List.nth (plots, studentIndex student)
    end
end
