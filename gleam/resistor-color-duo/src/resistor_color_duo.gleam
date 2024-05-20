pub type Color {
  Black
  Brown
  Red
  Orange
  Yellow
  Green
  Blue
  Violet
  Grey
  White
}

pub fn value(colors: List(Color)) -> Result(Int, Nil) {
  case colors {
    [c1, c2, ..] -> Ok(10 * code(c1) + code(c2))
    _ -> Error(Nil)
  }
}

fn code(color) {
  [Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White]
  |> do_code(color, 0)
}

fn do_code(colors, color, idx) {
  case colors {
    [] -> -1
    [c, ..] if c == color -> idx
    [_, ..rest] -> do_code(rest, color, idx + 1)
  }
}
