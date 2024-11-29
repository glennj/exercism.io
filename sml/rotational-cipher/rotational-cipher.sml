fun rotate (shiftKey: int) (text: string): string =
  let val alphabet = explode "abcdefghijklmnopqrstuvwxyz"
      val shift    = shiftKey mod length alphabet
      val rotated  = List.drop (alphabet, shift) @ List.take (alphabet, shift)

      val alphabet = alphabet @ List.map Char.toUpper alphabet
      val rotated  = rotated  @ List.map Char.toUpper rotated
      val mapping  = ListPair.zip (alphabet, rotated)

      fun translate c =
        case List.find (fn (a, _) => a = c) mapping
          of SOME (_, b) => b
           | NONE        => c

  in  String.map translate text
  end
