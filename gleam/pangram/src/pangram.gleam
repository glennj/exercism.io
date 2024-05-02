import gleam/int
import gleam/string

pub fn is_pangram(sentence: String) -> Bool {
  case to_int(string.lowercase(sentence), 0) {
    0b11_1111_1111_1111_1111_1111_1111 -> True // all 26 letters found
    _ -> False
  }
}

fn to_int(str: String, acc: Int) -> Int {
  case string.pop_grapheme(str) {
    Error(Nil) -> acc
    Ok(#(char, rest)) ->
      case index(char) {
        i if i < 0 || i > 25 -> acc
        i ->
          int.bitwise_shift_left(1, i)
          |> int.bitwise_or(acc)
      }
      |> to_int(rest, _)
  }
}

fn index(char: String) -> Int {
  let assert [ascii, ..] = string.to_utf_codepoints(char)
  string.utf_codepoint_to_int(ascii) - 97 // ascii "a"
}
