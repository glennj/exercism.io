import gleam/list
import gleam/string

pub fn build(letter: String) -> String {
  letter_to_int(letter)
  |> diamond(0, [])
  |> list.map(string.concat)
  |> string.join("\n")
}

fn diamond(n, i, bottom_half) {
  let row = mirror(right_half_row(n, i, []))
  case n == i {
    True -> mirror([row, ..bottom_half])
    False -> diamond(n, i + 1, [row, ..bottom_half])
  }
}

fn right_half_row(n, i, acc) {
  let len = list.length(acc)
  case len == n + 1, len == i {
    True, _ -> list.reverse(acc)
    False, True -> right_half_row(n, i, [int_to_letter(i), ..acc])
    False, False -> right_half_row(n, i, [" ", ..acc])
  }
}

fn mirror(bottom_half) {
  [list.reverse(bottom_half), list.drop(bottom_half, 1)]
  |> list.concat()
}

fn ascii_value(letter) {
  let assert Ok(cp) =
    letter
    |> string.to_utf_codepoints()
    |> list.first()
  string.utf_codepoint_to_int(cp)
}

fn letter_to_int(letter) {
  ascii_value(letter) - ascii_value("A")
}

fn int_to_letter(i) {
  let assert Ok(cp) = string.utf_codepoint(i + ascii_value("A"))
  string.from_utf_codepoints([cp])
}
